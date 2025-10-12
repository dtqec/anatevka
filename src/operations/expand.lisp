;;;; operations/expand.lisp
;;;;
;;;; EXPAND un-contracts a contracted node manufactured by CONTRACT.  Consider
;;;; the earlier example:
;;;;
;;;;     [  BLOSSOM   ]
;;;;     [    B ==> C ]
;;;;     [   ^      ~ ]
;;;;     [  /       ~ ]
;;;; ==> [ A        0 ]
;;;;     [  \       ~ ]
;;;;     [   v      ~ ]
;;;;     [    D ==> E ]
;;;;
;;;; Suppose that a future AUGMENT directive involves a vertex participating in
;;;; this contracted subgraph.  This changes the original matched edge entering
;;;; BLOSSOM at A to some other edge BLOSSOM == F for an external vertex F.  The
;;;; duty of EXPAND-BLOSSOM is to modify the matches within this contracted sub-
;;;; graph to be consistent with this new match.  Depending on which internal
;;;; vertex the new match edge BLOSSOM == F is actually attached, we have the
;;;; cases:
;;;;
;;;; to A: F == A (i.e., the empty path) ,
;;;; to B: F == B -- C == E -- D == A    ,
;;;; to C: F == C -- B == A              ,
;;;; to D: F == D -- E == C -- B == A    ,
;;;; to E: F == E -- D == A              .
;;;;
;;;; These show that an alternating chain can always be drawn through the
;;;; contracted graph, no matter which vertex is targeted.  Additionally, any
;;;; vertices not appearing in the above trees themselves participate in pre-
;;;; existing matches, hence contribute inert matches after blossom dissolution.
;;;;
;;;; Aside from this major goal, this operation is again made complicated by
;;;; having to retain the tree structure "around" an expanding blossom: for
;;;; instance, any child edges attached to this blossom have to be re-attached
;;;; to the appropriate internal node.
;;;;
;;;; NOTE: This extension of the augmentation chain through the blossom as
;;;;       described above _need not_ happen when AUGMENT happens.  Instead, it
;;;;       can be arbitrarily deferred.  EXPAND's job is to finally make good on
;;;;       such a deferral.

(in-package #:anatevka)

;;;
;;; supervisor command definitions
;;;

(define-process-upkeep ((supervisor supervisor)) (START-EXPAND pong)
  "Sets up the expand procedure."
  (with-slots (source-root) pong
    (let ((targets (list source-root))
          (blossom (blossom-edge-source-node (car (message-pong-edges pong)))))
      (process-continuation supervisor
                            `(BROADCAST-LOCK ,targets)
                            `(CHECK-ROOTS ,targets)
                            `(CHECK-INNER-BLOSSOM ,blossom ,source-root)
                            `(EXPAND-INNER-BLOSSOM ,blossom)
                            `(CLEAR-HELD-BY-ROOTS ,targets)
                            `(BROADCAST-UNLOCK)
                            `(HALT)))))

(define-process-upkeep ((supervisor supervisor)) (CHECK-INNER-BLOSSOM blossom source-root)
  "When a supervisor is in charge of blossom expansion, we know that the blossom to be expanded is necessarily an inner blossom.  Why?  Because outer blossoms don't get an expand recommendation from the tree, and barbell blossoms are expanded by the dryad. However, as is common in this distributed algorithm, there are many opportunities for information to go stale, and so before we tell the blossom to expand itself, we double check that it is, in fact, an inner blossom, by checking its `internal-weight', `positive?', and `pistil' slots, as well as making sure that its root is as the supervisor expected, which ensures that everything is a-OK."
  (unless (process-lockable-aborting? supervisor)
    (sync-rpc (make-message-values :values '(internal-weight pistil positive?))
        ((internal-weight pistil positive?) blossom)
      (sync-rpc (make-message-root-path)
          (root-path-result blossom)
        (when (or (/= 0 internal-weight) pistil positive?
                  (not (address= source-root
                                 (blossom-edge-source-node (first root-path-result)))))
          (setf (process-lockable-aborting? supervisor) t))))))

(define-process-upkeep ((supervisor supervisor)) (EXPAND-INNER-BLOSSOM blossom)
  "Blossom expansion is handled by the blossom itself. This instruction just triggers the expansion routine remotely and waits for it to complete."
  (unless (process-lockable-aborting? supervisor)
    (sync-rpc (make-message-expand)
        (expand-result blossom)
      (declare (ignore expand-result))
      nil)))

;;;
;;; data frames
;;;

;;;
;;; message definitions
;;;

(defstruct (message-expand (:include message))
  "Sent from a SUPERVISOR to a BLOSSOM to cause it to pop.")

(defstruct (message-blossom-parent (:include message))
  "Calculates the highest blossom parent which contains the recipient and which is contained in STOP-BEFORE."
  (stop-before nil :type (or null address)))

(defstruct (message-replace-child (:include message))
  "Replaces any child edges attaching to OLD-CHILD with edges attaching to NEW-CHILD instead."
  (old-child nil :type address)
  (new-child nil :type address))

;;;
;;; message handlers
;;;

(define-message-handler handle-message-expand
    ((node blossom-node) (message message-expand))
  "Starts the procedure for popping a contracting blossom."
  (cond
    ((blossom-node-pistil node)
     (send-message (blossom-node-pistil node) message))
    ((null (blossom-node-petals node))
     (send-message (message-reply-channel message)
                   (make-message-rpc-done)))
    (t
     (log-entry :entry-type ':expanding-blossom
                :blossom node
                :petals (blossom-node-petals node)
                :match-edge (blossom-node-match-edge node))
     (process-continuation node `(EXPAND-BLOSSOM ,(message-reply-channel message))))))

(define-message-handler handle-message-blossom-parent
    ((node blossom-node) (message message-blossom-parent))
  "Calculates the topmost blossom which contains NODE, subject to the possible limitation that we not exceed STOP-BEFORE."
  (with-slots (reply-channel stop-before) message
    (cond
      ((and (not (null stop-before))
            (or (null (blossom-node-pistil node))
                (and (blossom-node-pistil node)
                     (typep (blossom-node-pistil node) 'blossom-edge))))
       (error "Bad BLOSSOM-PARENT request."))
      ((or (null (blossom-node-pistil node))
           (and (blossom-node-pistil node)
                (typep (blossom-node-pistil node) 'blossom-edge))
           (and stop-before
                (address= stop-before (blossom-node-pistil node))))
       (send-message reply-channel
                     (make-message-rpc-done :result (process-public-address node))))
      (t
       (send-message (blossom-node-pistil node) message)))))

(define-rpc-handler handle-message-replace-child
    ((node blossom-node) (message message-replace-child))
  "Replaces a child edge targeting a given node by an edge targeting another node."
  (with-slots (reply-channel old-child new-child) message
    (dolist (child-edge (blossom-node-children node))
      (when (address= old-child (blossom-edge-target-node child-edge))
        (setf (blossom-edge-target-node child-edge)
              new-child)))
    nil))

;;;
;;; blossom-node command definitions
;;;

;;; NOTE: at present, EXPAND is the odd operation out in that it is handled in
;;;       the blossom commands rather than in the SUPERVISOR commands or in the
;;;       message handlers. nonetheless, see the SUPERVISOR documentation for
;;;       a description of what's happening here.

(define-process-upkeep ((node blossom-node)) (EXPAND-BLOSSOM reply-channel)
  "Sets up the EXPAND stack frame."
  ;; NOTE: because sync-receive does some implicit coroutine junk, performing
  ;;       operations after it executes conditionally + not repeating ourselves
  ;;       requires us to do some unpleasant juggling with the local #'FINALIZE.
  ;;       this is potentially fragile, and at any rate it's a smell i'd want to
  ;;       eliminate in the ultimate form of the DSL.
  ;;
  ;; it doesn't make sense to expand unmatched blossoms (but the dryad might try)
  (unless (blossom-node-match-edge node)
    (log-entry :entry-type ':aborting-expand-blossom
               :reason ':mateless-blossom
               :blossom node)
    (send-message reply-channel (make-message-rpc-done))
    (finish-handler))
  (labels ((finalize (matched-node root-node)
             ;; determines path from `root-node' to `matched-node', so
             ;; should `:key' on `source-node' of the `petals'
             (multiple-value-bind (path reversed? full-path)
                 (find-even-arm (blossom-node-petals node)
                                root-node matched-node
                                :key #'blossom-edge-source-node
                                :test #'address=
                                :rev #'reverse-blossom-edges)
               (declare (ignore reversed?))
               ;; set up the blossom expansion script
               (process-continuation node
                                     ;; we drop the first edge of `full-path' because that
                                     ;; edge has `matched-node' as it's source
                                     `(EXPAND-BLOSSOM-ADD-CYCLE-MATCHES ,(rest full-path))
                                     `(EXPAND-BLOSSOM-BUILD-TREE-PATH ,path 0)
                                     `(EXPAND-BLOSSOM-ATTACH-PARENT ,root-node)
                                     `(EXPAND-BLOSSOM-ATTACH-MATCH ,matched-node ,root-node)
                                     `(EXTINGUISH-BLOSSOM ,reply-channel)))))
    ;; determine the which vertex is matched external to the blossom
    (sync-rpc (make-message-blossom-parent
               :stop-before (process-public-address node))
        (matched-node (blossom-edge-source-vertex (blossom-node-match-edge node)))
      (cond
        ((blossom-node-parent node)
         ;; determine which vertex is attached to the blossom's parent
         (sync-rpc (make-message-blossom-parent
                    :stop-before (process-public-address node))
             (root-node (blossom-edge-source-vertex (blossom-node-parent node)))
           (finalize matched-node root-node)))
        (t
         (finalize matched-node matched-node))))))

(define-process-upkeep ((node blossom-node)) (EXPAND-BLOSSOM-BUILD-TREE-PATH path index)
  "This command walks the path from `root-node'->`matched-node', establishing all the parent and child relationships and setting `positive?' accordingly.  The parent/child relationship between the blossom's parent and `root-node' is established separatedly in `EXPAND-BLOSSOM-ATTACH-PARENT'. The parent/child relationship between the blossom's match and `matched-node', as well as the `positive?' setting of `matched-node', is implemented separately in the `EXPAND-BLOSSOM-ATTACH-MATCH' command.

                      root                   root
                        |                      |
                       b0               (NIL) b0
                       / \                   /  \
                      /   \                 v   \
                     b1   b4           (T) b1   b4
                     |     |               |     |
                     |  B  |    becomes    |  B  |
                     |     |               v     |
                     b2---b3         (NIL) b2---b3
                    //                    //
                   //                    //
                  a                     a

In the above example, `path' would be (`b0-->b1', `b1-->b2'), and so we build the parent/child relationships `b0-->b1' and `b1-->b2', and we set `positive?' of (b0, b1, b2) to be (NIL, T, NIL)."
  (unless (endp path)
    (let ((edge (pop path)))
      (process-continuation node `(EXPAND-BLOSSOM-BUILD-TREE-PATH ,path ,(1+ index)))
      ;; source `positive?' should be NIL, bc it's the parent of its match
      (sync-rpc (make-message-set :slots (if (evenp index)
                                             '(children positive?)
                                             '(children))
                                  :values (if (evenp index)
                                              `((,(copy-blossom-edge edge))
                                                ,nil)
                                              `((,(copy-blossom-edge edge)))))
          (set-result (blossom-edge-source-node edge))
        ;; target `positive?' should be T, bc it's the child of its match
        (sync-rpc (make-message-set :slots (if (evenp index)
                                               '(parent positive?)
                                               '(parent))
                                    :values (if (evenp index)
                                                `(,(reverse-blossom-edge edge)
                                                  ,t)
                                                `(,(reverse-blossom-edge edge))))
            (set-result (blossom-edge-target-node edge))
          nil)))))

(define-process-upkeep ((node blossom-node)) (EXPAND-BLOSSOM-ADD-CYCLE-MATCHES full-path)
  "This command detaches all of the blossom children from their `pistil'  (except for the `matched-node', which is handled separately in the command `EXPAND-BLOSSOM-ATTACH-MATCH'). In addition, it establishes all the `match-edge' relationships in the cycle, both in the alternating tree and for lone barbells.

                      root                   root
                        |                      |
                       b0                     b0
                       / \                   //
                      /   \                 //
                     b1   b4               b1   b4
                     |     |                    ||
                     |  B  |    becomes         ||
                     |     |                    ||
                     b2---b3               b2   b3
                    //                    //
                   //                    //
                  a                     a

In the above example, `full-path' would originally be (`b2-->b3', `b3-->b4', `b4-->b0', `b0-->b1', `b1-->b2'), but the first entry is trimmed in the calling command, and then we drop two more every iteration, so the edges that are actually considered in this command are `b3-->b4' and `b0-->b1', which are exactly the edges that should be matched. Additionally, that list of edges contains all the vertices that do not participate in the `a==>b2' matched edge, so we are confident that everybody is ridden of their blossom parent."
  ;; NOTE: This command also handles incrementing the `internal-weight's of all
  ;;       the blossom's children by the blossom's `internal-weight'. However,
  ;;       this functionality has not been thoroughly tested.

  ;; we jump two edges each time, because a single node can't have two matches
  (loop :for edge :in full-path :by #'cddr
        :for source-rx := (register)
        :for target-rx := (register)
        ;; drop `pistil' and establish `match-edge'
        :do (send-message (blossom-edge-source-node edge)
                          (make-message-set :reply-channel source-rx
                                            :slots '(pistil match-edge positive?)
                                            :values `(,nil ,(copy-blossom-edge edge) ,t)))
            (send-message (blossom-edge-target-node edge)
                          (make-message-set :reply-channel target-rx
                                            :slots '(pistil match-edge positive?)
                                            :values `(,nil ,(reverse-blossom-edge edge) ,t)))
        :nconc (list source-rx target-rx) :into rx-channels
        :finally (with-replies (replies) rx-channels
                   nil)))

(define-process-upkeep ((node blossom-node)) (EXPAND-BLOSSOM-ATTACH-PARENT root-node)
  "This command takes the blossom's parent relationship and pushes it onto the petal node that is the source vertex of the relationship. This should be performed whenever the blossom has a parent, even if `root-node' is equal to `matched-node'.

Here are two example configurations:
 
       r ---> [b0    ]                 r ---> [b0    ] ===> a
              [| \   ]                        [| \   ]
              [|  \  ]                        [|  \  ]
              [|   b1]   <--- blossom B --->  [|   b1]
              [|  /  ]                        [|  /  ]
              [| /   ]                        [| /   ]
       a <=== [b2    ]                        [b2    ]

left diagram:                      right diagram:
   `parent-edge' is `B:b0--->r'       `parent-edge' is `B:b0--->r'
   `root-node' is b0                  `root-node' is b0
   `matched-node' is b2               `matched-node' is b0

Note that in the right diagram, b0 is both the `root-node' and the `matched-node', because it is the vertex attached to both r and a."
  (when (blossom-node-parent node)
    (let ((parent-edge (copy-blossom-edge (blossom-node-parent node))))
      ;; here, we set the source-node of `parent-edge' to equal `root-node'
      ;; which in our above example is b0. thus, `parent-edge' is now `b0--->r'
      (setf (blossom-edge-source-node parent-edge) root-node)
      ;; then, we tell b0 that `parent-edge' should be its parent
      (sync-rpc (make-message-set :slots '(parent positive?)
                                  :values `(,parent-edge ,nil))
          (set-result root-node)
        ;; then, we tell r to replace it child relationship `r-->b0:B'
        ;; with the relationship `r--->b0' via `message-replace-child'
        (sync-rpc (make-message-replace-child
                   :old-child (process-public-address node)
                   :new-child root-node)
            (replace-result (blossom-edge-target-node parent-edge))
          nil)))))

(define-process-upkeep ((node blossom-node)) (EXPAND-BLOSSOM-ATTACH-MATCH matched-node root-node)
  "This command takes the blossom's match relationship and pushes it onto the petal vertex that is the source vertex of the relationship.

       r ---> [b0    ]                 r ---> [b0    ] ===> a
              [| \   ]                        [| \   ]
              [|  \  ]                        [|  \  ]
              [|   b1]   <--- blossom B --->  [|   b1]
              [|  /  ]                        [|  /  ]
              [| /   ]                        [| /   ]
       a <=== [b2    ]                        [b2    ]

left diagram:                      right diagram:
   `edge' is `B:b2===>a'              `edge' is `B:b0===>a'
   `root-node' is b0                  `root-node' is b0
   `matched-node' is b2               `matched-node' is b0

In the right diagram, b0 is both the `root-node' and the `matched-node', because it is the vertex attached to both r and a. For the follow-on comments below, we will use the left diagram labels."
  (let* ((edge (copy-blossom-edge (blossom-node-match-edge node)))
         (rx-channels nil))
    ;; here we change `edge' from `B:b2===>a' to `b2===>a'
    (setf (blossom-edge-source-node edge) matched-node)
    ;; then, we tell b2 that its new `match-edge' is `edge', that it has no
    ;; `pistil' anymore, and that it is a negative/odd/inner node
    (push (send-message matched-node
                        (make-message-set
                         :reply-channel (register)
                         :slots '(match-edge pistil positive?)
                         :values `(,(copy-blossom-edge edge)
                                   ,nil
                                   ,(or (not (blossom-node-parent node))
                                        (and
                                         (address= matched-node root-node)
                                         (address= (blossom-edge-target-node (blossom-node-parent node))
                                                   (blossom-edge-target-node edge)))))))
          rx-channels)
    ;; then, we tell a that its `match-edge' is `edge' in reverse
    (push (send-message (blossom-edge-target-node edge)
                        (make-message-set :reply-channel (register)
                                          :slots '(match-edge)
                                          :values `(,(reverse-blossom-edge edge))))
          rx-channels)
    ;; if B has no children, we're done
    (when (blossom-node-children node)
      ;; then, if B has a child (in our picture this is true: it's a)
      ;; we tell b2 that it needs the child relationship `b2--->a'
      (push (send-message matched-node
                          (make-message-set :reply-channel (register)
                                            :slots '(children)
                                            :values `((,(copy-blossom-edge edge)))))
            rx-channels)
      ;; finally, we tell a that `a--->b2' should be its parent
      (push (send-message (blossom-edge-target-node edge)
                          (make-message-set :reply-channel (register)
                                            :slots '(parent)
                                            :values `(,(reverse-blossom-edge edge))))
            rx-channels))
    ;; await replies
    (with-replies (replies) rx-channels
      nil)))

(define-process-upkeep ((node blossom-node)) (EXTINGUISH-BLOSSOM reply-channel)
  "Tell this blossom process to die."
  (send-message (blossom-node-dryad node)
                (make-message-remove-macrovertex :address (process-public-address node)))
  ;; NOTE: There's no data frame to pop.
  (when reply-channel
    (send-message reply-channel (make-message-rpc-done)))
  (log-entry :entry-type ':blossom-extinguished
             :log-level 2
             :blossom node)
  (setf (blossom-node-wilting node) t))
