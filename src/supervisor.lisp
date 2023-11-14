;;;; supervisor.lisp
;;;;
;;;; At the completion of a BLOSSOM-NODE's SCAN started via SCAN-LOOP, it is
;;;; left holding a minimal proposed action for the blossom to take. In order to
;;;; actually enact that action, it spawns a `SUPERVISOR' to take the reins and
;;;; submits to a passive role in the procedure from then on. Accordingly,
;;;; (almost) all of the logic in performing blossom algorithm operations is
;;;; bound up in the procedure definitions for SUPERVISORs.
;;;;
;;;; The operations on the blossom trees---the meat of the "manipulation" phase
;;;; of the algorithm, once the decision-making is through---is spurred by
;;;; messages sent from a `SUPERVISOR', which functions as a coordinating
;;;; process.
;;;;
;;;; The `AUGMENT', `GRAFT', and `REWEIGHT' operations are sufficient to solve
;;;; the minimum-weight perfect matching problem for bipartite weighted graphs.
;;;; In the non-bipartite case, however, it is possible to apply these
;;;; operations and reach a nonoptimal dead end.  The two operations
;;;; `CONTRACT' and `EXPAND' provide the remaining tools to, respectively,
;;;; reduce the nonbipartite case to the bipartite case and to port a bipartite
;;;; solution to a solution for the original graph.

(in-package #:anatevka)

(defclass supervisor (process-lockable)
  ((process-clock-rate :initform *blossom-node-clock-rate*)
   (node-class
    :documentation "The class identifier who spawned this SUPERVISOR."
    :accessor supervisor-node-class
    :initform 'blossom-node
    :initarg :node-class
    :type symbol)
   (node-dryad
    :accessor supervisor-node-dryad
    :initform nil
    :initarg :node-dryad
    :type (or null address)
    :documentation "The address of the host `DRYAD' for the node that spawned us."))
  (:documentation "A companion process responsible for coordinating a tree operation."))

(define-message-dispatch supervisor
  ;; nothing. supervisors are bull-headed.
  )

;;;
;;; supervisor data frame
;;;

(defstruct data-frame-supervisor
  "Data frame associated to the basic functioning on a SUPERVISOR process.

PONG: The PONG that this process received at its START."
  (pong nil :type message-pong))

;;;
;;; supervisor command definitions
;;;

(defgeneric supervisor-command-from-recommendation (recommendation)
  (:documentation "Converts a PONG recommendation to a supervisor procedure jump point.")
  (:method (recommendation)
    (error "Recommendation ~a has no associated jump point." recommendation))
  (:method ((recommendation (eql ':AUGMENT)))
    'START-AUGMENT)
  (:method ((recommendation (eql ':GRAFT)))
    'START-GRAFT)
  (:method ((recommendation (eql ':CONTRACT)))
    'START-CONTRACT)
  (:method ((recommendation (eql ':EXPAND)))
    'START-EXPAND)
  (:method ((recommendation (eql ':HOLD)))
    'START-MULTIREWEIGHT))

(define-process-upkeep ((supervisor supervisor) now) (START)
  "Set up initial state: the stack frame and which procedure to branch on."
  (let ((pong (pop (process-data-stack supervisor))))
    (with-slots (edges weight source-root target-root recommendation) pong
      (log-entry :entry-type 'got-recommendation
                 :source-root source-root
                 :recommendation recommendation
                 :weight weight
                 :edges edges)
      (let ((frame (make-data-frame-supervisor :pong pong)))
        (push frame (process-data-stack supervisor))
        (cond
          ;; If we get an invalid-weight recommendation, we use `CHECK-PONG'
          ;; to make sure it is not a result of stale commands or messages.
          ;; If the recommendation is cancelled, great. Otherwise, we follow up
          ;; with `ENSURE-ABORTING' which crashes the algorithm if `CHECK-PONG'
          ;; doesn't cause this supervisor to abort.
          ((minusp weight)
           (log-entry :entry-type 'invalid-recommendation :weight weight)
           (process-continuation supervisor
                                 `(CHECK-PONG ,pong)
                                 `(ENSURE-ABORTING ,pong)
                                 `(HALT)))
          ((zerop weight)
           (let ((jump-point (supervisor-command-from-recommendation recommendation)))
             (process-continuation supervisor `(,jump-point ,pong))))
          ;; reweighting case
          ((plusp weight)
           (process-continuation supervisor `(START-REWEIGHT ,pong)))
          (t
           (error "Unknown error when unpacking recommendation of weight ~a" weight)))))))

(define-process-upkeep ((supervisor supervisor) now) (HALT)
  "Stop the current `SUPERVISOR' and announce whether it's been a success. Additionally, unpause the `SOURCE-ROOT' so that it can start scanning again."
  (with-slots (pong) (pop (process-data-stack supervisor))
    (with-slots (source-root) pong
      (sync-rpc (make-message-set :slots '(paused?) :values `(,nil))
          ;; set `returned?' to handle RTSes gracefully as we
          ;; (intentionally) don't have an aborting? guard
          (set-result source-root :returned? returned?)
        (log-entry :entry-type 'success
                   :success (not (process-lockable-aborting? supervisor)))
        (process-die)))))

;;;
;;; sanity checks
;;;

(define-process-upkeep ((supervisor supervisor) now) (CHECK-ROOTS roots)
  "Ensure that these nodes are still actually unmatched roots."
  (unless (process-lockable-aborting? supervisor)
    (flet ((payload-constructor ()
             (make-message-values :values '(parent pistil match-edge))))
      (with-replies (values-lists) (send-message-batch #'payload-constructor roots)
        (loop :for (parent pistil match-edge) :in values-lists
              :do (when (or parent pistil match-edge)
                (setf (process-lockable-aborting? supervisor) t)))))))

(define-process-upkeep ((supervisor supervisor) now) (CHECK-PONG stale-pong)
  "Ensure that two locked trees still agree that this is a responsible weightless operation."
  (unless (process-lockable-aborting? supervisor)
    (with-slots (target-vertex source-node)
        (car (last (message-pong-edges stale-pong)))
      ;; compute the local weight, blossom, root at the near vertex
      (sync-rpc (make-message-soft-adjoin-root
                 ;; NOTE: this hard-coded pong type is OK, because it's never
                 ;;       going to be fed to `UNIFY-PONGS'.
                 :pong (make-message-pong :source-root (aether::make-address)
                                          :weight 0
                                          :edges (list (make-blossom-edge)))
                 :ping (make-message-soft-ping :weight 0
                                               :blossom (aether::make-address)))
          (local-pong
           (blossom-edge-source-vertex
            (car (last (message-pong-edges stale-pong))))
           :message-type message-pong :message-unpacker identity)
        (log-entry :entry-type 'local-pong-recommendation
                   :recommendation (message-pong-recommendation local-pong))
        ;; send a ping to the far vertex with this local weight
        (let ((local-blossom (blossom-edge-target-node
                              (car (last (message-pong-edges local-pong)))))
              (local-root (message-pong-target-root local-pong)))
          (sync-rpc (make-message-soft-ping :weight (message-pong-weight local-pong)
                                            :root local-root
                                            :blossom local-blossom
                                            :id (message-pong-source-id stale-pong))
              (replica-pong target-vertex
               :returned? returned? :message-type message-pong :message-unpacker identity)
            (when returned?
              (setf (process-lockable-aborting? supervisor) t)
              (finish-with-scheduling))
            (setf (blossom-edge-source-node
                   (car (last (message-pong-edges replica-pong))))
                  local-blossom
                  (blossom-edge-source-vertex
                   (car (last (message-pong-edges replica-pong))))
                  (blossom-edge-source-vertex
                   (car (last (message-pong-edges stale-pong)))))
            (process-continuation supervisor 
                                  `(EVALUATE-CHECK-PONG ,stale-pong ,local-pong
                                                        ,replica-pong))))))))

(define-process-upkeep ((supervisor supervisor) now)
    (EVALUATE-CHECK-PONG stale-pong local-pong replica-pong)
  "CHECK-PONG results in a refreshed REPLICA-PONG, which we're to compare against STALE-PONG and LOCAL-PONG, aborting if they differ in a way that indicates stale information."
  (setf (process-lockable-aborting? supervisor)
        (not (pong= stale-pong replica-pong))))

(define-process-upkeep ((supervisor supervisor) now) (ENSURE-ABORTING pong)
  "This command is used upon encountering a negatively-weighted edge rec, to cause the algorithm to crash if the recommendation doesn't resolve itself."
  (unless (process-lockable-aborting? supervisor)
    (with-slots (weight) pong
      (error "Running CHECK-PONG didn't fix a negative-weight edge: ~a" pong))))

(define-process-upkeep ((supervisor supervisor) now)
    (BROADCAST-PINGABILITY targets new-type)
  "Instruct the trees rooted at `TARGETS' to change their pingability to `NEW-TYPE'."
  (unless (process-lockable-aborting? supervisor)
    (flet ((payload-constructor ()
             (make-message-broadcast-pingability :ping-type new-type)))
      (with-replies (replies) (send-message-batch #'payload-constructor targets)
        nil))))
