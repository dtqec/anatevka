;;;; logger.lisp
;;;;
;;;; Structured logging and log processing.

(in-package #:anatevka)

;;;
;;; pretty-printing mechanisms
;;;

(defmethod print-log-entry (entry
                            (source dryad)
                            (entry-type (eql ':handling-sow))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] sowed ~a ~a at ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':type)
          (getf entry ':address)
          (getf entry ':id)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':got-recommendation))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] got recommendation ~a ~a ~{~a~^ ~} from ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':recommendation)
          (getf entry ':weight)
          (getf entry ':edges)
          (getf entry ':source-root)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':reweighting))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] reweighting roots ~a by ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':roots)
          (getf entry ':weight)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':success))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] closing with ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (if (getf entry ':success) "success" "failure")))

(defmethod print-log-entry (entry
                            (source blossom-node)
                            (entry-type (eql ':set-up-blossom))
                            &optional (stream *standard-output*))
  "Log entry for when a blossom node finishes setting itself up."
  (format stream "~5f: BLOSSOM ~a completed setting up (peduncle: ~a; match: ~a; children: ~{~a~^ ~}; parent: ~a; petals: ~{~a~^ ~}; pistil: ~a)~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':peduncle-edge)
          (getf entry ':match-edge)
          (getf entry ':children)
          (getf entry ':parent)
          (getf entry ':petals)
          (getf entry ':pistil)))

;;;
;;; filtering routines
;;;

(defun supervisor-logs-for-address (address &optional (logger *logger*))
  "Trims log messages to only ones related to SUPERVISOR actions involving `ADDRESS'."
  (let (entries relevant-supervisors)
    (dolist (entry (reverse (logger-entries logger)) (reverse entries))
      (cond
        ((and (typep (getf entry ':source) 'supervisor)
              (eql ':got-recommendation (getf entry ':entry-type))
              (or (and (not (null (getf entry ':source-root)))
                       (address= address (getf entry ':source-root)))
                  (and (not (null (not (null (getf entry ':target-root)))))
                       (address= address (getf entry ':target-root)))))
         (push (getf entry ':source) relevant-supervisors)
         (push entry entries))
        ((and (typep (getf entry ':source) 'supervisor)
              (member (getf entry ':source) relevant-supervisors))
         (push entry entries))))))

(defun successful-supervisors (entries)
  "Collects addresses of supervisors which either complete successfully or fail to complete at all."
  (loop :for entry :in entries
        :when (and (typep (getf entry ':source) 'supervisor)
                   (eql ':success (getf entry ':entry-type))
                   (eql T (getf entry ':success)))
          :collect (getf entry ':source) :into positive-processes
        :when (and (typep (getf entry ':source) 'supervisor)
                   (eql ':got-recommendation (getf entry ':entry-type))
                   (eql ':HOLD (getf entry ':recommendation))
                   (address=
                    (blossom-edge-source-node (first (getf entry ':edges)))
                    (blossom-edge-target-node (first (getf entry ':edges)))))
          :collect (getf entry ':source) :into self-held-processes
        :when (and (typep (getf entry ':source) 'supervisor)
                   (eql ':command (getf entry ':entry-type))
                   (eql ':START (getf entry ':command)))
          :collect (getf entry ':source) :into start-processes
        :when (and (typep (getf entry ':source) 'supervisor)
                   (eql ':success (getf entry ':entry-type)))
          :collect (getf entry ':source) :into done-processes
        :finally (return (union (set-difference positive-processes self-held-processes)
                                (set-difference start-processes done-processes)))))

(defgeneric algorithmic-entry? (entry source)
  (:documentation "Used to define which subset of `ENTRY' types emanating from `SOURCE' are critical to understanding algorithmic developments.")
  (:method (entry source) nil))

(defmethod algorithmic-entry? (entry (source supervisor))
  (member (getf entry ':entry-type) '(:got-recommendation
                                      :success
                                      :reweighting
                                      :rewinding
                                      :multireweighting)))

(defun reduced-log (&optional (logger *logger*))
  "Trims log messages to only ones of primary interest (see `ALGORITHMIC-ENTRY?')."
  (let (entries
        (successful-processes (successful-supervisors (logger-entries logger))))
    (dolist (entry (reverse (logger-entries logger)) (reverse entries))
      (let ((source (getf entry ':source)))
        (cond
          ((and (typep source 'supervisor)
                (member source successful-processes)
                (algorithmic-entry? entry source))
           (push entry entries))
          ((or (and (eql ':message-wilt (type-of (getf entry ':payload))))
               (and (eql ':spawned-fresh-blossom (getf entry ':entry-type)))
               (and (eql ':blossom-extinguished (getf entry ':entry-type))))
           (push entry entries))
          ;; dryad logs
          ((and (typep (getf entry ':source) 'dryad)
                ;; dryad sowing
                (or (eql ':handling-sow (getf entry ':entry-type))
                    (eql ':handling-sprout (getf entry ':entry-type))
                    (eql ':processing-pair (getf entry ':entry-type))
                    ;; dryad expansion
                    (and (eql ':command (getf entry ':entry-type))
                         (eql ':send-expand (getf entry ':command)))
                    (eql ':dryad-sending-expand (getf entry ':entry-type))))
           (push entry entries)))))))

(defun print-reduced-log (&optional (logger *logger*))
  "Shorthand for printing the results of `REDUCED-LOG."
  (print-log (reduced-log logger)))
