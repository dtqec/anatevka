;;;; lock.lisp
;;;;
;;;; Specialized blossom behavior for locking.

(in-package #:anatevka)

;;;
;;; blossom-node command definitions
;;;

(defmethod process-lockable-targets ((node blossom-node))
  (mapcar #'blossom-edge-target-node (union (blossom-node-petals node)
                                            (blossom-node-children node))))

(define-process-upkeep ((node blossom-node) now)
    (aether::%FINISH-UNLOCK)
  (setf (blossom-node-pingable node) ':ALL)
  (setf (blossom-node-held-by-roots node) nil)
  (when (process-lockable-done-signal node) ; signal := destroy? && ! aborting?
    (setf (blossom-node-parent node)    nil
          (blossom-node-children node)  nil
          (blossom-node-positive? node) t))
  (call-next-method))

;;;
;;; blossom-node handlers
;;;

(define-message-handler handle-message-lock
    ((node blossom-node) (message message-lock) now)
  "Prepares a BLOSSOM-NODE to be locked."
  (when (blossom-node-wilting node)
    (send-message (message-reply-channel message)
                  (make-message-rpc-done :result nil))
    (finish-handler))
  (unless (process-lockable-locked? node)
    (setf (blossom-node-pingable node) ':NONE))
  (call-next-method))

;;;
;;; supervisor command definitions
;;;

(define-process-upkeep ((supervisor supervisor) now)
    (BROADCAST-UNLOCK &key destroy? &allow-other-keys)
  "Cleans up after BROADCAST-LOCK."
  (with-slots (aborting? done-signal downward-rx-latches downward-tx-latches upward-tx-latch) supervisor
    (setf done-signal (and destroy? (not aborting?)))
    (send-message-batch (a:curry #'make-message-unlock :result done-signal)
                        downward-tx-latches
                        :replies? nil)
    (with-replies (replies) downward-rx-latches
      ;; NOTE: the setting of `aborting?' to NIL was removed because it clobbers
      ;;       the supervisor's notion of being aborted for other reasons, and
      ;;       confuses `HALT'
      (setf downward-tx-latches nil
            downward-rx-latches nil)
      (when upward-tx-latch
        (send-message upward-tx-latch (make-message-rpc-done :result t))))))
