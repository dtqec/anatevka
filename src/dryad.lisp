;;;; dryad.lisp
;;;;
;;;; These processes are responsible for farming the blossoms participating in
;;;; the matching computation.  They perform three services:
;;;;
;;;; (1) They spawn a blossom process for each vertex in the problem graph.
;;;; (2) When a blossom vertex needs a list of vertices to PING, they provide a
;;;;     list of relevant addresses.
;;;; (3) When a blossom vertex reaches a stable match, it plucks the blossoms
;;;;     from the computation and records those edges.

(in-package #:anatevka)

;;;
;;; definition of the DRYAD data structure
;;;

(defparameter *dryad-default-clock-rate* 10)

(defclass dryad (process-lockable)
  ((process-clock-rate
    :initform *dryad-default-clock-rate*)
   (match-address
    :accessor dryad-match-address
    :initarg :match-address
    :type address
    :documentation "The `ADDRESS' to which the `DRYAD' will send REAP messages.")
   (node-class
    :accessor dryad-node-class
    :initarg :node-class
    :initform 'blossom-node
    :type symbol
    :documentation "The class identifier for nodes that this `DRYAD' works with. Can be a `BLOSSOM-NODE' or any subclass of `BLOSSOM-NODE'.")
   ;; local state
   (ids
    :accessor dryad-ids
    :initform (make-hash-table :hash-function #'hash-address :test #'address=)
    :type hash-table
    :documentation "A map ADDRESS -> ID which records the id of a `BLOSSOM-NODE' instance.")
   (sprouted?
    :accessor dryad-sprouted?
    :initform (make-hash-table :hash-function #'hash-address :test #'address=)
    :type hash-table
    :documentation "A map ADDRESS -> BOOLEAN which records whether a `BLOSSOM-NODE' has begun participating in matches."))
  (:documentation "PROCESS responsible for the injection and ejection of nodes from the blossom algorithm."))

;;;
;;; passive DRYAD message handlers
;;;

(define-message-handler handler-message-sow
    ((dryad dryad) (message message-sow) now)
  "Adjoin a new node to the problem graph.

NOTE: In the basic implementation, these messages must be waiting for the DRYAD on launch."
  (let* ((node-id (message-sow-id message))
         (node-process (spawn-process (dryad-node-class dryad)
                                      :dryad (process-public-address dryad)
                                      :id node-id))
         (node-address (process-public-address node-process)))
    (schedule node-process now)
    (setf (gethash node-address (dryad-ids       dryad)) node-id
          (gethash node-address (dryad-sprouted? dryad)) nil)))

(define-message-handler handler-message-discover
    ((dryad dryad) (message message-discover) now)
  "Handles a DISCOVER message, sent by a BLOSSOM-NODE which expects a list of other BLOSSOM-NODE addresses to which it should send PINGs."
  (let ((channels
          (loop :for address :being :the :hash-keys :of (dryad-ids dryad)
                :unless (address= address (message-discover-address message))
                  :collect address)))
    (send-message (message-reply-channel message)
                  (make-message-discovery :channels-to-try channels))))

(define-message-handler handler-message-sprout
    ((dryad dryad) (message message-sprout) now)
  "Handles a SPROUT message, indicating that a BLOSSOM-NODE has been matched (for the first time)."
  (with-slots (address) message
    (when (gethash address (dryad-ids dryad))
      (setf (gethash address (dryad-sprouted? dryad)) t))))

(define-rpc-handler handler-message-wilting
    ((dryad dryad) (message message-wilting) now)
  "Handles a wilting message, indicating that a BLOSSOM-NODE is dying."
  (with-slots (address) message
    (let ((id (gethash address (dryad-ids dryad))))
      (remhash address (dryad-ids       dryad))
      (remhash address (dryad-sprouted? dryad))
      id)))

;;;
;;; install the handlers into the dispatch table
;;;

(define-message-dispatch dryad
  (message-sow      'handler-message-sow)
  (message-sprout   'handler-message-sprout)
  (message-discover 'handler-message-discover)
  (message-wilting  'handler-message-wilting))

;;;
;;; DRYAD command definitions
;;;

(define-process-upkeep ((dryad dryad) now) (START)
  "Start listening for ripe sprouted pairs."
  (process-continuation dryad `(SPROUTS-LOOP)))

(define-process-upkeep ((dryad dryad) now) (SPROUTS-LOOP)
  "Loop over sprouted nodes, looking for ripe pairs."
  ;; if not everyone is sprouted, hold off
  (when (loop :for sprouted? :in (a:hash-table-values (dryad-sprouted? dryad))
              :always (not sprouted?))
    (process-continuation dryad `(SPROUTS-LOOP))
    (finish-with-scheduling))
  (let ((addresses (a:hash-table-keys (dryad-sprouted? dryad))))
    (flet ((payload-constructor ()
             (make-message-values :reply-channel (register)
                                      :values '(match-edge))))
      (with-replies (replies) (send-message-batch #'payload-constructor addresses)
        ;; make sure everyone has a match. any that doesn't is in a blossom
        ;; which needs to be expanded.
        (loop :for address :in addresses
              :for reply :in replies
              :when (null (first reply))
                :do (process-continuation dryad
                                          `(SEND-EXPAND ,address)
                                          `(SPROUTS-LOOP))
                    (finish-with-scheduling))
        ;; all clear!
        (let ((emitted-addresses nil)
              (pairs nil))
          (dolist (reply replies)
            (let* ((left-address (blossom-edge-source-vertex (first reply)))
                   (right-address (blossom-edge-target-vertex (first reply)))
                   (left-member (member left-address emitted-addresses :test #'address=))
                   (right-member (member right-address emitted-addresses :test #'address=))
                   (ids (list (gethash left-address  (dryad-ids dryad))
                              (gethash right-address (dryad-ids dryad)))))
              (cond
                ((and left-member right-member)
                 nil)
                ((and (not left-member) (not right-member))
                 (push left-address emitted-addresses)
                 (push right-address emitted-addresses)
                 (push ids pairs))
                (t
                 (error "Two distinct match edges laid claim to the same vertex.")))))
          (process-continuation dryad
                                `(PROCESS-PAIRS ,pairs)
                                `(WIND-DOWN)))))))

(define-process-upkeep ((dryad dryad) now) (PROCESS-PAIRS pairs)
  "Iterates through `PAIRS' and sends corresponding REAP messages."
  (dolist (pair pairs)
    (send-message (dryad-match-address dryad)
                  (make-message-reap :ids pair))))

(define-process-upkeep ((dryad dryad) now) (SEND-EXPAND sprout)
  "Directs SPROUT to perform blossom expansion."
  (unless (process-lockable-aborting? dryad)
    ;; if we directly send the sprout a blossom-expand message, it will
    ;; automatically forward that up to its topmost blossom parent, which'll
    ;; then pop.  this isn't always desirable: we only want to expand those
    ;; blossoms which aren't participating in a tree.  so, instead, we
    ;; calculate the topmost blossom separately, send it a VALUES query,
    ;; and directly expand it if appropriate.
    (sync-rpc (make-message-blossom-parent)
        (topmost sprout)
      (sync-rpc (make-message-values :values '(children parent match-edge))
          ((children parent match-edge) topmost)
        (when (or children parent)
          (log-entry :entry-type 'aborting-dryad-expansion
                     :reason 'tree-structure))
        (unless (or children parent)
          (log-entry :entry-type 'dryad-sending-expand
               :sprout sprout
               :topmost topmost
               :match-edge match-edge)
          (sync-rpc (make-message-expand)
              (expand-reply topmost)
            nil))))))

(define-process-upkeep ((dryad dryad) now) (WIND-DOWN &optional (counter 50))
  (unless (zerop counter)
    (process-continuation dryad `(WIND-DOWN ,(1- counter)))))
