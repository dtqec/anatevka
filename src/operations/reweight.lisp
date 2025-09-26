;;;; operations/reweight.lisp
;;;;
;;;; If no weightless edge is available, then a blossom tree will modify its
;;;; internal weights so as to produce one.  One thinks of the internal weight
;;;; assigned to a (positive) node as a kind of "scanning radius": starting with
;;;; the graph
;;;;
;;;; [A +0] --2-- [B +0]
;;;;
;;;; with two unweighted vertices separated by an edge of weight 2, B might
;;;; reweight by 2, depicted like
;;;;
;;;;                  ^^^^
;;;;                 /    \
;;;;                /      \
;;;;               /        \
;;;; [A +0] --0-- <  [B +2]  >
;;;;               \        /
;;;;                \      /
;;;;                 \    /
;;;;                  vvvv
;;;;
;;;; so that A sits at the 'edge' of its newly enlarged radius and the edge
;;;; connecting them can be used to perform some other blossom tree operation.
;;;;
;;;; If the tree which is reweighting is more complex, the reweighting value is
;;;; applied by alternatingly increasing and decreasing the weight. For example,
;;;; if we begin with the tree
;;;;
;;;;    ^^^^              ^^^^
;;;;   /    \            /    \
;;;;  /      \          /      \
;;;; < [A +1] > --0--> < [B +1] > ==0==> [C +0]
;;;;  \      /          \      /
;;;;   \    /            \    /
;;;;    vvvv              vvvv
;;;;
;;;; and then reweight by 1, the effect is
;;;;
;;;;     ^^^^
;;;;    /    \                            ^^^^
;;;;   /      \                          /    \
;;;;  /        \                        /      \
;;;; <  [A +2]  > --0--> [B +0] ==0==> < [C +1] >
;;;;  \        /                        \      /
;;;;   \      /                          \    /
;;;;    \    /                            vvvv
;;;;     vvvv
;;;;
;;;; Critically, the reweighting operation changes the weights of those edges
;;;; _external_ to the tree, but all of the edges _internal_ to the tree remain
;;;; weightless because of the alternation.

(in-package #:anatevka)

;;;
;;; supervisor data frame
;;;

(defstruct data-frame-reweight
  "Data frame associated to a `SUPERVISOR' process enacting `REWEIGHT'.

`PONG': The `message-pong' passed to this `SUPERVISOR' upon spawn.

`TARGETS': The lock targets collected as part of this operation."
  (pong    nil :type message-pong)
  (targets nil :type list))

;;;
;;; supervisor command definitions
;;;

(define-process-upkeep ((supervisor supervisor)) (START-REWEIGHT pong)
  "Sets up the reweight procedure by first gathering lock targets."
  (push (make-data-frame-reweight :pong pong) (process-data-stack supervisor))
  (process-continuation supervisor
                        `(GATHER-TARGETS)
                        `(START-INNER-REWEIGHT)
                        `(FINISH-REWEIGHT)))

(define-process-upkeep ((supervisor supervisor)) (GATHER-TARGETS)
  "Before entering the critical section, we must determine which roots we should lock. If our target-root is not NIL, we need to lock it and the hold cluster it is contained within (if any). NB: This is only relevant to non-weightless operations (reweight and multireweight)."
  (unless (process-lockable-aborting? supervisor)
    (with-slots (pong targets) (peek (process-data-stack supervisor))
      (with-slots (source-root target-root root-bucket) pong
        (push source-root targets)
        ;; the contents of `targets' depend on the recommendation. it always
        ;; includes the `source-root', and additionally
        ;;  - `AUGMENT': the `target-root' (and potentially its hold cluster)
        ;;  - `GRAFT': one end of the barbell
        ;;  - `EXPAND' or `CONTRACT': nothing
        ;;  - `HOLD': same as `AUGMENT' + other unique roots in `root-bucket'
        (log-entry :entry-type ':gathering-targets
                   :pong (copy-message-pong pong)
                   :source-root source-root
                   :target-root target-root
                   :root-bucket root-bucket
                   :targets targets)
        (unless (null target-root)
          (sync-rpc (make-message-convergecast-collect-roots)
              (target-cluster target-root :returned? returned?)
            (setf targets (remove-duplicates
                           (append (list source-root target-root)
                                   target-cluster
                                   root-bucket)
                           :test #'address=))
            (log-entry :entry-type ':collected-target-cluster
                       :source-root source-root
                       :target-root target-root
                       :target-cluster target-cluster
                       :root-bucket root-bucket
                       :targets targets)))))))

(define-process-upkeep ((supervisor supervisor)) (START-INNER-REWEIGHT)
  "The reweight 'critical section':

1. Lock the `TARGETS'.
2. Check that the `source-root' is still a root.
3. Change the pingability of the `TARGETS' to `:SOFT'.
4. Check for a weightless edge. Abort if found.
5. Change the pingability of the `TARGETS' to `:NONE'.
6. Reweight the `source-root' by the `weight' of the `pong'.
7. Change the pingability of the `TARGETS' to `:SOFT'.
8. Check if reweighting the `source-root' resulted in a negative-weight edge.
    a. If so, and this is the second time we've been here, rewind fully.
    b. Otherwise, if so, rewind the reweighting by half and go back to (7).
9. Unlock the `TARGETS'.
"
  (with-slots (pong targets) (peek (process-data-stack supervisor))
    (with-slots (source-root weight) pong
      (process-continuation supervisor
                            `(BROADCAST-LOCK ,targets)
                            `(CHECK-ROOTS (,source-root))
                            `(BROADCAST-PINGABILITY ,targets :SOFT)
                            `(CHECK-REWEIGHT (,source-root) ,pong)
                            `(BROADCAST-PINGABILITY ,targets :NONE)
                            `(BROADCAST-REWEIGHT (,source-root) ,weight)
                            `(BROADCAST-PINGABILITY ,targets :SOFT)
                            `(CHECK-REWINDING (,source-root) ,pong 0)
                            `(BROADCAST-UNLOCK)
                            `(HALT)))))

(define-process-upkeep ((supervisor supervisor)) (CHECK-REWEIGHT roots original-pong)
  "Because `CHECK-PONG' doesn't do a global check, we potentially can end up with a reweighting when we shouldn't. This fixes that by making sure that there are no lower-weight recommendations available before we begin reweighting."
  (unless (process-lockable-aborting? supervisor)
    (let ((check-pong nil)
          (original-weight (message-pong-weight original-pong))
          (original-rec (message-pong-recommendation original-pong))
          (original-target (message-pong-target-root original-pong)))
      (flet ((payload-constructor ()
               (make-message-soft-scan :weight 0
                                       :internal-roots roots
                                       :strategy ':STAY)))
        (log-entry :entry-type ':checking-reweight
                   :log-level 1
                   :roots roots
                   :weight original-weight)
        (with-replies (replies
                       :message-type message-pong
                       :message-unpacker identity)
                      (send-message-batch #'payload-constructor roots)
          (loop :for reply :in replies :unless (null reply)
                :do (setf check-pong (unify-pongs check-pong reply)))
          (let ((check-pong-rec (message-pong-recommendation check-pong))
                (check-pong-weight (message-pong-weight check-pong))
                (check-pong-edges (message-pong-edges check-pong))
                (check-pong-target (message-pong-target-root check-pong)))
            (log-entry :entry-type ':check-reweight-details
                       :log-level 1
                       :roots roots
                       :weight original-weight
                       :check-pong-rec check-pong-rec
                       :check-pong-weight check-pong-weight
                       :check-pong-edges check-pong-edges
                       :check-pong-target check-pong-target)
            (when (< check-pong-weight original-weight)
              (log-entry :entry-type ':check-reweight-aborting-lower-weight
                         :log-level 1
                         :original-weight original-weight
                         :check-pong-weight check-pong-weight)
              (setf (process-lockable-aborting? supervisor) t))
            (when (and (not (address= original-target check-pong-target))
                       (not (eql check-pong-rec original-rec))
                       (eql ':hold check-pong-rec))
              (log-entry :entry-type ':check-reweight-aborting-lower-precedence
                         :log-level 1
                         :original-rec original-rec
                         :original-target original-target
                         :check-pong-rec check-pong-rec
                         :check-pong-target check-pong-target)
              (setf (process-lockable-aborting? supervisor) t))))))))

(define-process-upkeep ((supervisor supervisor))
    (BROADCAST-REWEIGHT roots weight)
  "Instruct some `ROOTS' to reweight their trees by `WEIGHT'."
  (unless (process-lockable-aborting? supervisor)
    (log-entry :entry-type ':reweighting
               :log-level 2
               :weight weight
               :roots roots)
    (flet ((payload-constructor ()
             (make-message-broadcast-reweight :weight weight)))
      (with-replies (replies) (send-message-batch #'payload-constructor roots)
        (log-entry :entry-type ':reweighting-finished
                   :log-level 1
                   :weight weight
                   :roots roots)))))

(define-process-upkeep ((supervisor supervisor))
    (CHECK-REWINDING roots original-pong carry)
  "Instruct a set of `ROOTS' to ensure that their reweighting has not resulted in an erroneous global state. If they have, then we want to rewind the reweighting, by using the `BROADCAST-REWEIGHT' command."
  (unless (process-lockable-aborting? supervisor)
    ;; NOTE: we couldn't call MAKE-PONG even if we wanted to, since we don't
    ;;       have access to the underlying node's Lisp object (or its type).
    (let ((rewinding-pong nil)
          (original-amount (message-pong-weight original-pong)))
      (flet ((payload-constructor ()
               (make-message-soft-scan :weight 0 :strategy ':STAY)))
        (log-entry :entry-type ':checking-rewinding
                   :log-level 1
                   :roots roots
                   :weight original-amount)
        (with-replies (replies
                       :returned? returned?
                       :message-type message-pong
                       :message-unpacker identity)
                      (send-message-batch #'payload-constructor roots)
          (loop :for reply :in replies :unless (null reply)
                :do (setf rewinding-pong (unify-pongs rewinding-pong reply)))
          ;; The `maximum-rewinding' variable tracks how much is left to
          ;; potentially rewind from the initial recommendation reweighting.
          (let ((maximum-rewinding (- original-amount carry))
                (minimum-weight-edge (message-pong-weight rewinding-pong))
                (rewinding-pong-rec (message-pong-recommendation rewinding-pong))
                (rewinding-pong-edges (message-pong-edges rewinding-pong))
                (rewinding-pong-source (message-pong-source-root rewinding-pong)))
            (log-entry :entry-type ':check-rewinding-details
                       :log-level 1
                       :roots roots
                       :minimum-weight-edge minimum-weight-edge
                       :rewinding-pong-rec rewinding-pong-rec
                       :rewinding-pong-edges rewinding-pong-edges
                       :rewinding-pong-source rewinding-pong-source)
            (when (minusp minimum-weight-edge)
              ;; When we encounter a negative-weight edge, this means that
              ;; our reweighting operation happened at the same time as another
              ;; nearby reweighting operation. We could fully backtrack and
              ;; deweight by the original recommendation, but this can result
              ;; in livelock scenarios when the state of the problem graph is
              ;; sufficiently symmetric. Fortunately, the `minimum-weight-edge'
              ;; value that we get back from our soft-scan check gives us some
              ;; useful information -- it is bounded above by the value of the
              ;; smallest simultaneous reweight in the local area. If we instead
              ;; backtrack by half that amount, we allow nearby alternating trees
              ;; to grow heavier (and thus closer to one another), while still
              ;; maintaining the validity of nearby modified edge weights,
              ;; thus breaking the symmetries of the problem graph and avoiding
              ;; livelock induced by repeated reweighting and rewinding.
              ;; However, we don't want to halve indefinitely, so we only do
              ;; that for the first round of rewinding.
              (let ((rewinding-amount minimum-weight-edge))
                ;; If it is our first try, attempt to compromise with a nearby
                ;; simultaneous reweighter by using half the overlap weight.
                (when (zerop carry)
                  (setf rewinding-amount (/ rewinding-amount 2)))
                (let ((new-carry (- carry rewinding-amount)))
                  (log-entry :entry-type ':rewinding
                             :log-level 2
                             :roots roots
                             :amount (- rewinding-amount)
                             :overall new-carry)
                  ;; If the rewinding amount is such that we are not fully
                  ;; backtracking, check ourselves again and respond accordingly.
                  (when (< (- rewinding-amount) maximum-rewinding)
                    (process-continuation supervisor
                                          `(BROADCAST-PINGABILITY ,roots :SOFT)
                                          `(CHECK-REWINDING ,roots ,original-pong ,new-carry)))
                  ;; If we get a rewinding of larger magnitude than the initial
                  ;; recommendation, then we should not do that. In fact, the
                  ;; cumulative rewinding should be carried from rewinding to
                  ;; rewinding so that it doesn't cause problems.
                  (when (> (- rewinding-amount) maximum-rewinding)
                    (setf rewinding-amount (- maximum-rewinding)))
                  ;; Finally, add the commands for actually doing the rewind.
                  (process-continuation supervisor
                                        `(BROADCAST-PINGABILITY ,roots :NONE)
                                        `(BROADCAST-REWEIGHT ,roots ,rewinding-amount)))))))))))

(define-process-upkeep ((supervisor supervisor)) (FINISH-REWEIGHT)
  "Clean up after the local state of the reweight operation."
  (pop (process-data-stack supervisor)))

;;;
;;; message definitions
;;;

(defstruct (message-broadcast-reweight (:include message))
  "Sent from a `SUPERVISOR' to a tree to reweight its top-level nodes by `WEIGHT'."
  (weight nil :type real))

;;;
;;; message handlers
;;;

(define-broadcast-handler handle-message-broadcast-reweight
    ((node blossom-node) (message message-broadcast-reweight))
  "Increments the `INTERNAL-WEIGHT' of `NODE' by the `WEIGHT' of the `MESSAGE', and then instructs `NODE's children to reweight themselves by the additive inverse of `WEIGHT'."
  (with-slots (weight) message
    (with-slots (internal-weight) node
      (incf internal-weight weight)
      (log-entry :entry-type ':reweight-details
                 :amount weight
                 :new-internal-weight internal-weight)
      (when (minusp internal-weight)
        (log-entry :entry-type ':negative-internal-weight
                   :internal-weight internal-weight))
      (setf weight (- weight))
      (push-broadcast-frame :targets (mapcar #'blossom-edge-target-node
                                             (blossom-node-children node))))))
