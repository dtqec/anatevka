;;;; operations/contract.lisp
;;;;
;;;; In the case that the endpoints of a weightless edge both belong to the same
;;;; tree, it is inappropriate to perform an augmentation: since the paths from
;;;; these nodes to their "respective roots" both end in the same place, one
;;;; cannot assign both of two possible new matches to the root.  Instead, the
;;;; appropriate action is to contract this subgraph into a single vertex.  This
;;;; has the effect that any _later_ augmentation path in which this contracted
;;;; vertex participates can be lifted to an augmentation path through the
;;;; original graph: such a subgraph always contains an odd number of nodes,
;;;; hence between any pair of nodes there is a path with an even number of
;;;; edges, given either by following the original cycle of edges clockwise
;;;; or counterclockwise.
;;;;
;;;; As an example, consider the following partial tree and its weightless edge:
;;;;
;;;;        B ==> C
;;;;       ^      ~
;;;;      /       ~
;;;; ==> A        0
;;;;      \       ~
;;;;       v      ~
;;;;        D ==> E
;;;;
;;;; By tracing a path from C and from E to their root, one produces a cycle of
;;;; odd length in which C ~~ E and (the beginnings of) their paths to the root
;;;; participate: [A, B, C, E, D].  CONTRACT replaces this by one node:
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
;;;; See EXPAND for a demonstration of the value of this tree operation.
;;;;
;;;; This operation is made complicated by having to track the tree structure
;;;; "around" the subgraph to be contracted: for instance, any other children
;;;; connected to vertices which are going to be contracted but which themselves
;;;; do not participate in the contraction.

(in-package #:anatevka)

;;;
;;; supervisor data frame
;;;

(defstruct data-frame-contract
  "Data frame associated to a SUPERVISOR process enacting CONTRACT.

PONG: The PONG that this processes received at its START.

PATH: The cycle of blossom child edges to install into this blossom.

PEDUNCLE-EDGE: The edge leading from the root of this subtree out of the graph.

FRESH-BLOSSOM: The address of the newly contracted blossom.

PETAL-CHILD-EDGES: The list of child edges attached to the blossoms in the subtree which will become children of the newly contracted blossom."
  (pong              nil :type message-pong)
  (path              nil :type list)
  (peduncle-edge     nil :type (or null blossom-edge))
  (fresh-blossom     nil :type address)
  (petal-child-edges nil :type list)
  ;; here's a surprising bug: BROADCAST-LOCK maintains a LIFO stack of locks.
  ;; since we lock the blossom petals before forming and locking the new blossom,
  ;; this means that the fresh blossom gets freed to act before its originally-
  ;; locked root does. if the fresh blossom immediately emits a scan directive,
  ;; then the locked root will never get around to fulfilling its unlock. so, we
  ;; stow the fresh lock here to release last.
  (stowed-rx-latch   nil :type (or null address))
  (stowed-tx-latch   nil :type (or null address)))

;;;
;;; supervisor command definitions
;;;

(define-process-upkeep ((supervisor supervisor)) (START-CONTRACT pong)
  "Begins the CONTRACT routine, sets up the stack frames."
  (with-slots (source-root) pong
    (let ((targets (list source-root)))
      ;; set up script
      (process-continuation supervisor
                            `(BROADCAST-LOCK ,targets)
                            `(CHECK-ROOTS ,targets)
                            `(BROADCAST-PINGABILITY ,targets :SOFT)
                            `(CHECK-PONG ,pong)
                            `(BROADCAST-PINGABILITY ,targets :NONE)
                            `(START-INNER-CONTRACT)
                            `(BROADCAST-UNLOCK)
                            `(HALT)))))

(define-process-upkeep ((supervisor supervisor)) (START-INNER-CONTRACT)
  "Begins the critical section of the CONTRACT routine."
  (unless (process-lockable-aborting? supervisor)
    (let* ((supervisor-frame (peek (process-data-stack supervisor)))
           (fresh-blossom (spawn-process (supervisor-node-class supervisor)
                                         :id (gensym "BLOSSOM")
                                         ;; prevent SCANs
                                         :paused? t
                                         :debug? (process-debug? supervisor))))
      (schedule fresh-blossom (now))
      (log-entry :entry-type ':spawned-fresh-blossom
                 :fresh-blossom fresh-blossom)
      (push (make-data-frame-contract
             :fresh-blossom (process-public-address fresh-blossom)
             :pong (data-frame-supervisor-pong supervisor-frame))
            (process-data-stack supervisor))
      (process-continuation supervisor
                            `(BROADCAST-LOCK ,(list (process-public-address fresh-blossom)))
                            `(STOW-LOCK)
                            `(COMPUTE-BLOSSOM-PATHS)
                            `(HANDLE-PISTIL)
                            `(HANDLE-PETALS)
                            `(HANDLE-BLOSSOM-SUB-CHILDREN)
                            `(HANDLE-NEW-BLOSSOM)
                            `(BROADCAST-UNLOCK) ; NOTE: double-calling BROADCAST-UNLOCK makes
                            `(RELEASE-STOWED-LOCK))))) ; the second one a NOP.

(define-process-upkeep ((supervisor supervisor)) (STOW-LOCK)
  (with-slots (downward-rx-latches downward-tx-latches) supervisor
    (with-slots (stowed-rx-latch stowed-tx-latch) (peek (process-data-stack supervisor))
      (setf stowed-rx-latch (pop downward-rx-latches)
            stowed-tx-latch (pop downward-tx-latches)))))

(define-process-upkeep ((supervisor supervisor)) (RELEASE-STOWED-LOCK)
  (with-slots (downward-rx-latches downward-tx-latches) supervisor
    (with-slots (stowed-rx-latch stowed-tx-latch) (pop (process-data-stack supervisor))
      (push stowed-rx-latch downward-rx-latches)
      (push stowed-tx-latch downward-tx-latches)
      (process-continuation supervisor `(BROADCAST-UNLOCK)))))

(define-process-upkeep ((supervisor supervisor)) (COMPUTE-BLOSSOM-PATHS)
  "This command computes the cycle which will constitute the fresh blossom.

                   G      <--      root     -->    G
                   |                               |
                   F                               F
                   |      <-- peduncle edge -->    |
                [  C  ]                           [C~~~]
                [ / \ ]                           [|  ~]
                [B   D]   <-- fresh blossom -->   [B  ~]
                [|   |]                           [|  ~]
                [A ~ E]                           [A~~~]

Both above tree diagrams are valid arrangments that would trigger this operation, but we will concern ourselves with the left one.

 (1) A `CONTRACT' 0 is recommended between A and E, so the first thing we do is determine the two paths from the root G to the nodes A and E:

 G -> A : (NIL G -> F F -> C C -> B B -> A)
 G -> E : (NIL G -> F F -> C C -> D D -> E)

 (2) Then, we want to trim everything higher in the tree than the first edge shared by the two root paths, so that we know what constitutes the cycle of edges in the fresh blossom we're about to create.

 tail A : (F -> C C -> B B -> A)
 tail E : (F -> C C -> D D -> E)

 (3) Then, we trim the first entry off of the tails, and we call that the peduncle as long as it non-null (it would be null if the tree root was C instead of F). This becomes the match and parent of the fresh blossom.

 peduncle edge  : F -> C
 trimmed tail A : (C -> B B -> A)
 trimmed tail E : (C -> D D -> E)

 (4) Finally, we combine the two trimmed tails (one of which is reversed) and the edge that suggested the contract blossom operation, to build the cycle.

 blossom cycle : (C -> B B -> A A -> E E -> D D -> C)"
  (let* ((frame (peek (process-data-stack supervisor)))
         (edge (car (message-pong-edges (data-frame-contract-pong frame))))
         (originating-node (blossom-edge-source-node edge))
         (recipient-node (blossom-edge-target-node edge))
         (rx-channels nil))
    ;; (1a) get chain from originating blossom node through to the root
    (push (send-message originating-node
                        (make-message-root-path :reply-channel (register)))
          rx-channels)
    ;; (1b) get chain from recipient blossom node through to the root
    (push (send-message recipient-node
                        (make-message-root-path :reply-channel (register)))
          rx-channels)
    (with-replies (root-paths) rx-channels
      ;; (2) prune shared ancestor edges above the peduncle
      (destructuring-bind (originating-tail recipient-tail)
          (latest-common-head (second root-paths) (first root-paths)
                              :key #'blossom-edge-target-node
                              :test #'address=)
        ;; (3) trim the peduncle from the tails, if present
        (cond
          ;; (3a) when both tails have a non-null first entry
          ;; this happens in both of the above tree diagrams
          ((and (first originating-tail) (first recipient-tail)
                (address= (blossom-edge-target-node (first originating-tail))
                          (blossom-edge-target-node (first recipient-tail))))
           (setf (data-frame-contract-peduncle-edge frame)
                 (or (first originating-tail) (first recipient-tail))
                 originating-tail (rest originating-tail)
                 recipient-tail (rest recipient-tail)))
          ;; (3b) when both tails have a null first entry
          ;; this happens when there is no peduncle edge, meaning that
          ;; the fresh blossom cycle includes the root of the tree
          ((and (null (first originating-tail))
                (null (first recipient-tail)))
           (setf originating-tail (rest originating-tail)
                 recipient-tail (rest recipient-tail))))
        ;; (4) finally, combine the peduncle-free tails to produce
        ;; the list of edges that makes up the fresh blossom cycle
        (setf (data-frame-contract-path frame)
              (append originating-tail
                      (list (copy-blossom-edge edge))
                      (reverse-blossom-edges recipient-tail)))))))

(define-process-upkeep ((supervisor supervisor)) (HANDLE-PISTIL)
  "Tell the source of the peduncle edge that the fresh blossom is its child.

                G      <--        root       -->    G
                |                                   |
                F                                   F
                |      <--   peduncle edge   -->    |
             [  C  ]                               [C~~~]
             [ / \ ]                               [|  ~]
             [B   D]   <-- fresh blossom (H) -->   [B  ~]
             [|   |]                               [|  ~]
             [A ~ E]                               [A~~~]

If we have a non-null peduncle edge (F -> C above), then we need to tell its source node (F) that it has a new child -- the fresh blossom (which we will call H). So, we send F a `message-attach-parent' message, which iterates through F's existing child relationships, and whichever one matches the peduncle edge has its target node updated from C to H:C, meaning that the target has node H and vertex C."
  (let ((frame (peek (process-data-stack supervisor))))
    (with-slots (peduncle-edge fresh-blossom) frame
      (when peduncle-edge
        (sync-rpc (make-message-attach-parent :fresh-blossom fresh-blossom
                                              :peduncle-edge peduncle-edge)
            (attach-result (blossom-edge-source-node peduncle-edge))
          nil)))))

(define-process-upkeep ((supervisor supervisor)) (HANDLE-PETALS)
  "Tell the blossom's petals what's up."
  (with-slots (path fresh-blossom petal-child-edges) (peek (process-data-stack supervisor))
    (let ((children (mapcar #'blossom-edge-target-node path)))
      (flet ((payload-constructor ()
               (make-message-convert-child-to-petal :fresh-blossom fresh-blossom)))
        (with-replies (petal-replies)
                      (send-message-batch #'payload-constructor children)
          (setf petal-child-edges
                (remove-if (lambda (x)
                             (member (blossom-edge-target-node x)
                                     (mapcar #'blossom-edge-target-node path)
                                     :test #'address=))
                           (apply #'append petal-child-edges petal-replies))))))))

(define-process-upkeep ((supervisor supervisor)) (HANDLE-BLOSSOM-SUB-CHILDREN)
  "Tell all the other children what's up."
  (with-slots (fresh-blossom petal-child-edges) (peek (process-data-stack supervisor))
    (let ((petal-children (mapcar #'blossom-edge-target-node petal-child-edges)))
      (flet ((payload-constructor ()
               (make-message-reattach-cycle-child :fresh-blossom fresh-blossom)))
        (with-replies (replies)
                      (send-message-batch #'payload-constructor petal-children)
          nil)))))

(define-process-upkeep ((supervisor supervisor)) (HANDLE-NEW-BLOSSOM)
  "Tell the blossom itself what's up."
  (let ((frame (peek (process-data-stack supervisor))))
    (with-slots (fresh-blossom peduncle-edge path petal-child-edges) frame
      ;; see `COMPUTE-BLOSSOM-PATHS' above to understand why the source node of
      ;; the first edge in the `path' corresponds to the node closest to the root
      (let ((rootmost-cycle-node (blossom-edge-source-node (first path))))
        ;; determine the `DRYAD' of that "rootmost" node in the cycle
        (sync-rpc (make-message-values :values '(dryad))
            ((dryad-address) rootmost-cycle-node)
          (setf (supervisor-node-dryad supervisor) dryad-address)
          (send-message dryad-address
                        (make-message-add-macrovertex :address fresh-blossom))
          (sync-rpc (make-message-set-up-blossom
                     :peduncle-edge peduncle-edge
                     :petals path
                     :petal-children petal-child-edges
                     :dryad dryad-address)
              (set-up-result fresh-blossom)
            (values)))))))

;;;
;;; message definitions
;;;

(defstruct (message-root-path (:include message))
  "Calculates the path from a node through to the root of its containing tree."
  (path nil :type list))

(defstruct (message-attach-parent (:include message))
  "Sent from a SUPERVISOR to a BLOSSOM-NODE to install a new PARENT."
  (fresh-blossom nil :type address)
  (peduncle-edge nil :type (or null blossom-edge)))

(defstruct (message-convert-child-to-petal (:include message))
  "Sent from a SUPERVISOR to a BLOSSOM-NODE to install a new CHILD."
  (fresh-blossom nil :type address))

(defstruct (message-reattach-cycle-child (:include message))
  "Sent from a SUPERVISOR to a BLOSSOM-NODE to transfer responsibility for a petal-child."
  (fresh-blossom nil :type address))

(defstruct (message-set-up-blossom (:include message))
  "Sent from a SUPERVISOR to a newly formed contracting BLOSSOM to set up its slots."
  (peduncle-edge  nil :type (or null blossom-edge))
  (petals         nil :type list)
  (petal-children nil :type list)
  (dryad          nil :type address))

;;;
;;; message handlers
;;;

(define-message-handler handle-message-root-path
    ((node blossom-node) (message message-root-path))
  "Calculates the path from a blossom through to the tree root (consisting only of toplevel blossoms)."
  (with-slots (path reply-channel) message
    (cond
      ((and (blossom-node-pistil node)
            (typep (blossom-node-pistil node) 'blossom-edge))
       (send-message reply-channel (make-message-rts)))
      ((blossom-node-pistil node)
       (send-message (blossom-node-pistil node) message))
      ((blossom-node-parent node)
       (push (reverse-blossom-edge (blossom-node-parent node)) path)
       (send-message (blossom-edge-target-node (blossom-node-parent node)) message))
      (t
       (send-message reply-channel
                     (make-message-rpc-done :result path))))))

(define-rpc-handler handle-message-attach-parent
    ((node blossom-node) (message message-attach-parent))
  "Attaches a fresh blossom to an existing parent."
  (with-slots (peduncle-edge reply-channel fresh-blossom) message
    (assert (not (null peduncle-edge)))
    (assert (not (null (blossom-node-children node))))
    (let (did-work-p)
      (dolist (child-edge (blossom-node-children node))
        (when (address= (blossom-edge-target-node child-edge)
                        (blossom-edge-target-node peduncle-edge))
          (setf (blossom-edge-target-node child-edge) fresh-blossom
                did-work-p t)))
      (assert did-work-p))
    (setf (blossom-node-match-edge node)
          (copy-blossom-edge peduncle-edge)
          (blossom-edge-target-node (blossom-node-match-edge node))
          fresh-blossom)
    nil))

(define-rpc-handler handle-message-convert-child-to-petal
    ((node blossom-node) (message message-convert-child-to-petal))
  "Attaches an old child to a new blossom as a petal."
  (with-slots (reply-channel fresh-blossom) message
    (prog1 (blossom-node-children node)
      (setf (blossom-node-positive? node)  t
            (blossom-node-parent node)     nil
            (blossom-node-pistil node)     fresh-blossom
            (blossom-node-match-edge node) nil
            (blossom-node-children node)   nil))))

(define-rpc-handler handle-message-reattach-cycle-child
    ((node blossom-node) (message message-reattach-cycle-child))
  "Attaches an old child to a new blossom as a (non-blossom-)child."
  (with-slots (reply-channel fresh-blossom) message
    (setf (blossom-edge-target-node (blossom-node-parent node))
          fresh-blossom)
    nil))

;; NOTE: this message is really hefty. you could cut it down somewhat by making
;;       the fresh blossom responsible for setting _itself_ up. this would also
;;       alleviate the obnoxious problem with locking/spawning timing.
(define-rpc-handler handle-message-set-up-blossom
    ((node blossom-node) (message message-set-up-blossom))
  "Sets up a new contracting blossom's slots."
  (with-slots (peduncle-edge petals petal-children dryad reply-channel) message
    (loop :for petal-child :in petal-children
          :for fresh-edge := (copy-blossom-edge petal-child)
          :do (setf (blossom-edge-source-node fresh-edge)
                    (process-public-address node))
          :collect fresh-edge :into fresh-edges
          :finally (setf petal-children fresh-edges))
    ;; if we have a peduncle edge, then the fresh blossom should have both a
    ;; match and a parent (which are identical). we build these edges by
    ;; reversing the peduncle edge, and then setting the source node of the
    ;; newly-created reversed edge to be the fresh blossom
    (when peduncle-edge
      (let ((match-and-parent-edge (reverse-blossom-edge peduncle-edge)))
        (setf (blossom-edge-source-node match-and-parent-edge)
              (process-public-address node)
              (blossom-node-match-edge node)
              match-and-parent-edge
              (blossom-node-parent node)
              (copy-blossom-edge match-and-parent-edge))))
    (setf (blossom-node-petals node)        petals
          (blossom-node-children node)      petal-children
          ;; assign this macrovertex a dryad
          (blossom-node-dryad node)         dryad
          ;; lastly, unpause our new blossom so that it can SCAN
          (blossom-node-paused? node)        nil)
    (log-entry :entry-type ':set-up-blossom
               :log-level 2
               :peduncle-edge peduncle-edge
               :match-edge (blossom-node-match-edge node)
               :children (blossom-node-children node)
               :parent (blossom-node-parent node)
               :petals (blossom-node-petals node)
               :pistil (blossom-node-pistil node)
               :dryad (blossom-node-dryad node))
    nil))
