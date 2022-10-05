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

`INTERNAL-PONG': The unified `message-pong' among the different roots in this `HOLD-CLUSTER', where all the roots in the cluster are treated as part of the same tree.  Ultimately serves to measure the amount by which to `MULTIREWEIGHT'."
  (hold-cluster  nil :type list)
  (internal-pong nil :type (or null message-pong)))

;;;
;;; supervisor command definitions
;;;

(define-process-upkeep ((supervisor supervisor) now) (START-MULTIREWEIGHT pong)
  "Sets up the multireweight procedure.

1. Collect the mutually held roots for the `HOLD-CLUSTER'
2. Lock the `HOLD-CLUSTER' and check the rootiness of each root.
3. Change the pingability of the cluster to `:SOFT'.
4. Scan the `HOLD-CLUSTER' for the best external rec to use for reweighting.
5. Reweight the `HOLD-CLUSTER' according to the recommendation.
6. Check to see if the `HOLD-CLUSTER' should be rewound, and do so if need be.
7. Unlock the targets and tear down transient state."
  ;; NOTE: we couldn't call MAKE-PONG even if we wanted to, since we don't have
  ;;       access to the underlying node's Lisp object (or its type).
  (push (make-data-frame-multireweight :internal-pong nil)
        (process-data-stack supervisor))
  (with-slots (root-bucket source-root) pong
    (setf root-bucket (remove-duplicates root-bucket :test #'address=))
    (process-continuation supervisor
                          `(CONVERGECAST-COLLECT-ROOTS ,(list source-root) ,root-bucket)
                          `(CHECK-PRIORITY ,source-root)
                          `(START-INNER-MULTIREWEIGHT)
                          `(FINISH-MULTIREWEIGHT)
                          `(HALT))))

(define-process-upkeep ((supervisor supervisor) now)
    (CONVERGECAST-COLLECT-ROOTS cluster roots)
  "Recursively collects the `HELD-BY-ROOTS' values of `ROOTS' to determine the set of roots that are participating in this `HOLD' cluster (meaning that they are mutually held by each other), starting with a base `CLUSTER' of just the `SOURCE-ROOT'. If any replies are NIL, we abort."
  (with-slots (hold-cluster) (peek (process-data-stack supervisor))
    (flet ((payload-constructor ()
             (make-message-convergecast-collect-roots :hold-cluster cluster)))
      (with-replies (replies :returned? returned?)
                    (send-message-batch #'payload-constructor roots)
        (when (some #'null replies)
          (log-entry :entry-type 'aborting-multireweight
                     :reason 'root-collection-failed
                     :hold-cluster cluster
                     :held-by-roots roots)
          (setf (process-lockable-aborting? supervisor) t)
          (finish-with-scheduling))
        (setf hold-cluster (reduce #'address-union (list* cluster replies)))))))

(define-process-upkeep ((supervisor supervisor) now) (CHECK-PRIORITY original-root)
  "Confirm that, of the roots in the hold cluster, we have priority to act.  Namely, we have priority when our `ORIGINAL-ROOT' carries the minimum ID of all the roots in the cluster."
  (with-slots (hold-cluster) (peek (process-data-stack supervisor))
    ;; don't bother _multi_reweighting if we're in a cluster of 1.
    (when (endp (rest hold-cluster))
     (log-entry :entry-type 'aborting-multireweight
                :reason 'cluster-of-one
                :hold-cluster hold-cluster)
     (setf (process-lockable-aborting? supervisor) t)
     (finish-with-futures))
    (sync-rpc (make-message-id-query)
        (original-id original-root)
      (with-replies (replies)
                    (send-message-batch #'make-message-id-query hold-cluster)
        (let ((cluster-id (reduce #'min-id replies)))
          (unless (equalp original-id (min-id original-id cluster-id))
            (setf (process-lockable-aborting? supervisor) t)))))))

(define-process-upkeep ((supervisor supervisor) now) (START-INNER-MULTIREWEIGHT)
  "This is the start of the \"critical segment\", where it begins to be impossible to rewind partway through the modifications we're about to make."
  (with-slots (hold-cluster) (peek (process-data-stack supervisor))
    (cond
      ((not (process-lockable-aborting? supervisor))
       (process-continuation supervisor
                             `(BROADCAST-LOCK ,hold-cluster)
                             `(CHECK-ROOTS ,hold-cluster)
                             `(BROADCAST-PINGABILITY ,hold-cluster :SOFT)
                             `(MULTIREWEIGHT-BROADCAST-SCAN ,hold-cluster)
                             `(BROADCAST-PINGABILITY ,hold-cluster :NONE)
                             `(MULTIREWEIGHT-BROADCAST-REWEIGHT ,hold-cluster)
                             `(BROADCAST-PINGABILITY ,hold-cluster :SOFT)
                             `(MULTIREWEIGHT-CHECK-REWINDING ,hold-cluster)
                             `(BROADCAST-UNLOCK))) ; don't destroy trees
      (t
       (log-entry :entry-type 'aborting-multireweight
                  :reason 'previously-aborted
                  :hold-cluster hold-cluster)
       nil))))

(define-process-upkeep ((supervisor supervisor) now)
    (MULTIREWEIGHT-BROADCAST-SCAN roots)
  "Now that we know the full `HOLD-CLUSTER', we `SCAN' each, and aggregate the results in order to make a reweighting decision."
  (unless (process-lockable-aborting? supervisor)
    (with-slots (internal-pong) (peek (process-data-stack supervisor))
      (flet ((payload-constructor ()
               (make-message-soft-scan :weight 0 :internal-roots roots :repeat? t)))
        (with-replies (replies
                       :returned? returned?
                       :message-type message-pong
                       :message-unpacker identity)
                      (send-message-batch #'payload-constructor roots)
          (loop :for reply :in replies :unless (null reply)
                :do (assert (not (minusp (message-pong-weight reply))))
                    (setf internal-pong
                          (unify-pongs internal-pong reply))))))))

(define-process-upkeep ((supervisor supervisor) now)
    (MULTIREWEIGHT-BROADCAST-REWEIGHT roots)
  "Having aggregated coordinated advice, we now enact it by sending individual reweight instructions to all the `ROOTS'. This is achieved via the `BROADCAST-REWEIGHT' command."
  (unless (process-lockable-aborting? supervisor)
    (with-slots (internal-pong) (peek (process-data-stack supervisor))
      (let ((amount (message-pong-weight internal-pong)))
        (log-entry :entry-type 'multireweighting
                   :recommendation (message-pong-recommendation internal-pong)
                   :amount amount
                   :roots roots)
        (process-continuation supervisor `(BROADCAST-REWEIGHT ,roots ,amount))))))

(define-process-upkeep ((supervisor supervisor) now)
    (MULTIREWEIGHT-CHECK-REWINDING roots)
  "Just as when we're reweighting, now we have to check to make sure we didn't create any negative-weight edges. We do so by pushing the `CHECK-REWINDING' command onto the command stack."
  (unless (process-lockable-aborting? supervisor)
    (with-slots (internal-pong) (peek (process-data-stack supervisor))
      (process-continuation supervisor `(CHECK-REWINDING ,roots ,internal-pong 0)))))

(define-process-upkeep ((supervisor supervisor) now) (FINISH-MULTIREWEIGHT)
  "Clean up after the local state of the multireweight operation."
  (pop (process-data-stack supervisor)))

;;;
;;; message definitions
;;;

(defstruct (message-convergecast-collect-roots (:include message))
  "Sent from a `SUPERVISOR' to a tree to collect a cluster of roots that are mutually held by each other. The `HOLD-CLUSTER' is the current `LIST' of `BLOSSOM-NODE' roots that are known to be mutually held up."
  (hold-cluster nil :type list))

;;;
;;; message handlers
;;;


(define-convergecast-handler handle-message-convergecast-collect-roots
    ((node blossom-node) (message message-convergecast-collect-roots) now)
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
