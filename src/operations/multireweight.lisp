;;;; operations/multireweight.lisp
;;;; 
;;;; It's possible for a pair (or more of trees to form where they each have a
;;;; nonpositive node of high weight and they each have a positive node sitting
;;;; on its boundary, so that they send each other mutual `HOLD' messages when
;;;; performing a `SCAN'.  This leads to deadlock (GH-143), unless the mutually-
;;;; blocked trees coordinate to perform a simultaneous reweight.  This is
;;;; implemented by the `MULTIREWEIGHT' procedure.
;;;;
;;;; Here's an example deadlocked forest configuration that requires a
;;;; simultaneous reweight to break:
;;;;
;;;;       ^
;;;;      / \
;;;;     /   \
;;;;    A  E==F
;;;;   /|\ ^ /
;;;;  / v \|/
;;;; C==B  D
;;;;  \   /
;;;;   \ /
;;;;    v
;;;;
;;;; We mean to indicate that the negative nodes E and B have weight d(E, B)/2,
;;;; which the positive nodes A, C, D, F are unweighted.  Neither tree can
;;;; reweight individually: if A grows, then the edge A--E will become negative,
;;;; and similarly if D grows then the edge D--B will become negative.  However,
;;;; if they coordinate, then A and D can acquire weight as B and E lose it.

(in-package #:anatevka)

(defun address-union (list1 list2)
  "Helper function for computing the union of two `LIST's of `ADDRESS'es."
  (union list1 list2 :test #'address=))

;;;
;;; supervisor data frame
;;;

(defstruct data-frame-multireweight
  "Data frame associated to a `SUPERVISOR' process enacting `MULTIREWEIGHT'.

`HOLD-CLUSTER': The aggregated set of mutually held roots, for which deadlock can only be broken via `MULTIREWEIGHT'.

`INTERNAL-PONG': The unified `message-pong' among the different roots in this `HOLD-CLUSTER', where all the roots in the cluster are treated as part of the same tree.  Ultimately serves to measure the amount by which to `MULTIREWEIGHT'.

`TARGETS': The lock targets collected as part of this operation.

`SOURCE-ROOT': The source-root of the originating HOLD pong.

`ROOT-BUCKET': The root-bucket of the originating HOLD pong.

`CLAIMED-ROOTS': Addresses of successfully-claimed roots."
  (hold-cluster  nil :type list)
  (internal-pong nil :type (or null message-pong))
  (targets       nil :type list)
  (source-root   nil :type address)
  (root-bucket   nil :type list)
  (claimed-roots nil :type list))

;;;
;;; supervisor command definitions
;;;

(define-process-upkeep ((supervisor supervisor)) (START-HOLD pong)
  "Sets up the multireweight procedure by first collecting all mutually-held roots, which form the `hold-cluster'.

After collecting the `hold-cluster', we then `CHECK-PRIORITY' to determine if we should proceed or abort. If we don't abort, then we move on to performing an 'internal scan' to figure out how to proceed."
  (with-slots (root-bucket source-root) pong
    ;; NOTE: we couldn't call MAKE-PONG even if we wanted to, since we don't have
    ;;       access to the underlying node's Lisp object (or its type).
    (push (make-data-frame-multireweight :internal-pong nil
                                         :source-root source-root
                                         :root-bucket (remove-duplicates
                                                       root-bucket :test #'address=))
          (process-data-stack supervisor))
    (process-continuation supervisor
                          `(GATHER-HOLD-CLUSTER)
                          `(CHECK-PRIORITY)
                          `(START-MULTIREWEIGHT)
                          `(FINISH-MULTIREWEIGHT)
                          `(HALT))))

(define-process-upkeep ((supervisor supervisor)) (GATHER-HOLD-CLUSTER)
  "Recursively collects the `held-by-roots' values of the pong's `root-bucket' to determine the set of roots that are participating in this `HOLD' cluster (meaning that they are mutually held by each other), starting with a base `cluster' of just the `source-root'. If any replies are NIL, we abort."
  (with-slots (source-root root-bucket hold-cluster) (peek (process-data-stack supervisor))
    (let ((cluster (list source-root)))
      (flet ((payload-constructor ()
               (make-message-convergecast-collect-roots :hold-cluster cluster)))
        (with-replies (replies :returned? returned?)
                      (send-message-batch #'payload-constructor root-bucket)
          (setf hold-cluster (reduce #'address-union (list* cluster replies)))
          (cond
            ((some #'null replies)
             (log-entry :entry-type ':aborting-multireweight-collection
                        :log-level 1
                        :source-root source-root
                        :root-bucket root-bucket)
             (setf (process-lockable-aborting? supervisor) t))
            ;; don't bother _multi_reweighting if we're in a cluster of 1.
            ((endp (rest hold-cluster))
             (log-entry :entry-type ':aborting-multireweight-solo
                        :log-level 1
                        :source-root source-root)
             (setf (process-lockable-aborting? supervisor) t))))))))

(define-process-upkeep ((supervisor supervisor)) (CHECK-PRIORITY)
  "Confirm that, of the roots in the hold cluster, we have priority to act. Namely, we have priority when our `source-root' carries the minimum ID (i.e. coordinate) of all the roots in the `hold-cluster'."
  (unless (process-lockable-aborting? supervisor)
    (with-slots (source-root hold-cluster) (peek (process-data-stack supervisor))
      ;; `hold-cluster' includes `source-root', so we begin by removing it
      (let ((cluster (remove source-root hold-cluster :test #'address=)))
        (sync-rpc (make-message-id-query) (source-id source-root)
          (with-replies (replies :returned? returned?)
                        (send-message-batch #'make-message-id-query cluster)
            (let ((cluster-min-id (reduce #'min-id replies)))
              (unless (equalp source-id (min-id source-id cluster-min-id))
                (log-entry :entry-type ':aborting-multireweight-priority
                           :log-level 1
                           :source-root source-root
                           :source-id source-id
                           :hold-cluster cluster
                           :cluster-min-id cluster-min-id)
                (setf (process-lockable-aborting? supervisor) t)))))))))

(define-process-upkeep ((supervisor supervisor)) (START-MULTIREWEIGHT)
  "Begins the multireweight procedure, by first claiming all the roots in the hold cluster, then scanning them to come up with the best internal pong, then collecting all the targets to lock, and finally entering the critical section."
  (unless (process-lockable-aborting? supervisor)
    (with-slots (source-root hold-cluster) (peek (process-data-stack supervisor))
      (process-continuation supervisor
                            `(CLAIM-ROOTS ,hold-cluster)
                            `(BROADCAST-SCAN-MULTIREWEIGHT ,hold-cluster)
                            `(GATHER-TARGETS-MULTIREWEIGHT)
                            `(START-INNER-MULTIREWEIGHT)
                            `(RELEASE-ROOTS)))))

(define-process-upkeep ((supervisor supervisor)) (CLAIM-ROOTS roots)
  "Claim all the provided `ROOTS'. If any claim fails, abort."
  (unless (process-lockable-aborting? supervisor)
    (with-slots (claimed-roots) (peek (process-data-stack supervisor))
      (with-replies (replies :returned? returned?)
                    (send-message-batch #'make-message-claim-root roots)
        (when (some #'null replies)
          (log-entry :entry-type ':aborting-multireweight-claim-failure
                     :log-level 1
                     :roots roots)
          (setf (process-lockable-aborting? supervisor) t))
        (setf claimed-roots (nconc claimed-roots (remove nil replies)))))))

(define-process-upkeep ((supervisor supervisor)) (BROADCAST-SCAN-MULTIREWEIGHT roots)
  "Now that we know the full `HOLD-CLUSTER', we `SCAN' each, and aggregate the results in order to make a reweighting decision."
  (unless (process-lockable-aborting? supervisor)
    (with-slots (internal-pong) (peek (process-data-stack supervisor))
      (flet ((payload-constructor ()
               (make-message-scan :weight 0 :internal-roots roots :strategy ':STAY)))
        (with-replies (replies
                       :returned? returned?
                       :message-type message-pong
                       :message-unpacker identity)
                      (send-message-batch #'payload-constructor roots)
          (loop :for reply :in replies :unless (null reply)
                :do (setf internal-pong
                          ;; internal-roots?
                          (unify-pongs internal-pong reply)))
          (with-slots (source-root source-id target-root weight edges recommendation)
              internal-pong
            (log-entry :entry-type ':multireweight-broadcast-scan-result
                       :log-level 1
                       :source-root source-root
                       :source-id source-id
                       :target-root target-root
                       :weight weight
                       :edges edges
                       :recommendation recommendation
                       :hold-cluster roots)
            (cond
              ;; if our best recommendation is negative, we abort the MRW
              ((minusp (message-pong-weight internal-pong))
               (log-entry :entry-type ':aborting-multireweight-negative-pong
                          :log-level 1
                          :source-root source-root
                          :source-id source-id
                          :target-root target-root
                          :weight weight
                          :edges edges
                          :recommendation recommendation
                          :hold-cluster roots)
               (setf (process-lockable-aborting? supervisor) t))
              ;; if our best recommendation is zero, we also abort the MRW
              ((zerop (message-pong-weight internal-pong))
               (log-entry :entry-type ':aborting-multireweight-zero-pong
                         :log-level 1
                         :source-root source-root
                         :source-id source-id
                         :target-root target-root
                         :weight weight
                         :edges edges
                         :recommendation recommendation
                         :hold-cluster roots)
               (setf (process-lockable-aborting? supervisor) t)))))))))

(define-process-upkeep ((supervisor supervisor)) (GATHER-TARGETS-MULTIREWEIGHT)
  "Before entering the critical section, we must determine which roots we should lock. If our target-root is outside of our hold-cluster, we need to lock it and the hold cluster it is contained within (if any)."
  (unless (process-lockable-aborting? supervisor)
    (with-slots (hold-cluster internal-pong targets) (peek (process-data-stack supervisor))
      (with-slots (source-root target-root) internal-pong
        (log-entry :entry-type ':gathering-targets-multireweight
                   :internal-pong (copy-message-pong internal-pong)
                   :source-root source-root
                   :target-root target-root
                   :hold-cluster hold-cluster
                   :targets targets)
        (cond
          ((member target-root hold-cluster :test #'address=)
           (setf targets hold-cluster))
          (t
           (sync-rpc (make-message-convergecast-collect-roots)
               (target-cluster target-root :returned? returned?)
             (setf targets (remove-duplicates
                            (append hold-cluster (list target-root) target-cluster)
                            :test #'address=))
             (log-entry :entry-type ':collected-target-cluster-multireweight
                        :source-root source-root
                        :target-root target-root
                        :target-cluster target-cluster
                        :hold-cluster hold-cluster
                        :targets targets))))))))

(define-process-upkeep ((supervisor supervisor)) (START-INNER-MULTIREWEIGHT)
  "Finally, we reach the 'critical section', where it becomes impossible to rewind partway through the modifications we're about to make:

1. Lock the `TARGETS' (the `HOLD-CLUSTER' and potentially an external `TARGET-ROOT').
2. Check that each root in the `HOLD-CLUSTER' is still a root.
3. Change the pingability of the `TARGETS' to `:SOFT'.
4. Check that our multireweight recommendation is still valid.
5. Change the pingability of the `TARGETS' to `:NONE'.
6. Reweight the `HOLD-CLUSTER' according to the recommendation.
7. Change the pingability of the `TARGETS' to `:SOFT'.
8. Check to see if the `HOLD-CLUSTER' should be rewound, and do so if need be.
9. Unlock the targets and tear down transient state."
  (unless (process-lockable-aborting? supervisor)
    (with-slots (hold-cluster internal-pong targets) (peek (process-data-stack supervisor))
      (with-slots (weight) internal-pong
        (process-continuation supervisor
                              `(BROADCAST-LOCK ,targets)
                              `(CHECK-ROOTS ,hold-cluster)
                              `(BROADCAST-PINGABILITY ,targets :SOFT)
                              `(CHECK-REWEIGHT ,hold-cluster ,internal-pong)
                              `(BROADCAST-PINGABILITY ,targets :NONE)
                              `(BROADCAST-REWEIGHT ,hold-cluster ,weight)
                              `(BROADCAST-PINGABILITY ,targets :SOFT)
                              `(CHECK-REWINDING ,hold-cluster ,internal-pong 0)
                              `(BROADCAST-UNLOCK))))))

(define-process-upkeep ((supervisor supervisor)) (RELEASE-ROOTS)
  "Release all the claimed roots."
  (with-slots (claimed-roots) (peek (process-data-stack supervisor))
    (log-entry :entry-type ':releasing-roots
               :claimed-roots claimed-roots)
    (with-replies (replies :returned? returned?)
                  (send-message-batch #'make-message-release-root claimed-roots)
      (log-entry :entry-type ':released-roots))))

(define-process-upkeep ((supervisor supervisor)) (FINISH-MULTIREWEIGHT)
  "Clean up supervisor-local state from the multireweight operation, by popping the `DATA-FRAME-MULTIREWEIGHT' from its data stack. Additionally, if the operation was successful, unset `HELD-BY-ROOTS' for everyone in the `HOLD-CLUSTER'."
  (with-slots (hold-cluster) (pop (process-data-stack supervisor))
    (unless (process-lockable-aborting? supervisor)
      (flet ((payload-constructor ()
               (make-message-set :slots '(held-by-roots) :values `(,nil))))
        (with-replies (replies)
                      (send-message-batch #'payload-constructor hold-cluster))))))

;;;
;;; message definitions
;;;

(defstruct (message-convergecast-collect-roots (:include message))
  "Sent from a `SUPERVISOR' to a tree to collect a cluster of roots that are mutually held by each other. The `HOLD-CLUSTER' is the current `LIST' of `BLOSSOM-NODE' roots that are known to be mutually held up."
  (hold-cluster nil :type list))

;;;
;;; message handlers
;;;

(define-convergecast-subordinate handle-message-convergecast-collect-roots
    ((node blossom-node) (message message-convergecast-collect-roots))
  "Check to see if we're held. If not, `RETURN-FROM-CAST' and send back up a NIL. If we are held, add ourselves to the `HOLD-CLUSTER'. Additionally, if we are held by `NEW-ROOTS' that aren't currently in the cluster, forward this message along to them to continue gathering roots. Finally, send the aggregated cluster back to the sender."
  (with-slots (hold-cluster reply-channel) message
    ;; If we're not held, abort the convergecast.
    (when (endp (blossom-node-held-by-roots node))
      (return-from-cast nil 'root-not-held))
    ;; Otherwise, see if we've grown the cluster and/or encountered new roots.
    (with-slots (held-by-roots) node
      (let* ((new-cluster (list* (process-public-address node) hold-cluster))
             (new-roots (set-difference held-by-roots new-cluster :test #'address=)))
        ;; If any reply is NIL or triggers a `RETURNED?' we want to abort the
        ;; whole operation, which we accomplish by sending back a NIL.
        (flet ((null-address-union (input replies)
                 (when (some #'null replies)
                   (return-from null-address-union nil))
                 (reduce #'address-union (list* input replies))))
          ;; If we are held by `NEW-ROOTS', this will forward the convergecast
          ;; along to them, aggregating results into `HOLD-CLUSTER'. Otherwise,
          ;; if `NEW-ROOTS' is NIL it will just send back the `NEW-CLUSTER'.
          (setf hold-cluster new-cluster)
          (push-convergecast-frame :handle-rts? t
                                   :func #'null-address-union
                                   :input new-cluster
                                   :targets new-roots))))))
