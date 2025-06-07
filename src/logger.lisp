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
  (format stream "~5f: [~a] sowed ~a at ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':address)
          (getf entry ':id)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':got-recommendation))
                            &optional (stream *standard-output*))
  (if (eql ':hold (getf entry ':recommendation))
      (format stream "~5f: [~a] got ~a ~a ~{~a~^ ~} from ~a w/ root-bucket ~a~%"
              (getf entry ':time)
              (getf entry ':source)
              (getf entry ':recommendation)
              (getf entry ':weight)
              (getf entry ':edges)
              (getf entry ':source-root)
              (getf entry ':root-bucket))
      (format stream "~5f: [~a] got ~a ~a ~{~a~^ ~} from ~a~%"
              (getf entry ':time)
              (getf entry ':source)
              (getf entry ':recommendation)
              (getf entry ':weight)
              (getf entry ':edges)
              (getf entry ':source-root))))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':reweighting))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] reweighting roots (~{~a~^ ~}) by ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':roots)
          (getf entry ':weight)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':rewinding))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] rewinding roots (~{~a~^ ~}) by ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':roots)
          (getf entry ':amount)))

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
  (format stream "~5f: [~a] completed setting up (peduncle: ~a; match: ~a; children: ~{~a~^ ~}; parent: ~a; petals: ~{~a~^ ~}; pistil: ~a)~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':peduncle-edge)
          (getf entry ':match-edge)
          (getf entry ':children)
          (getf entry ':parent)
          (getf entry ':petals)
          (getf entry ':pistil)))

(defmethod print-log-entry (entry
                            (source blossom-node)
                            (entry-type (eql ':blossom-extinguished))
                            &optional (stream *standard-output*))
  "Log entry for when a blossom node finishes setting itself up."
  (format stream "~5f: [~a] expanded and extinguished~%"
          (getf entry ':time)
          (getf entry ':source)))

(defmethod print-log-entry (entry
                            (source dryad)
                            (entry-type (eql ':dryad-sending-expand))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] sending EXPAND to ~a (topmost: ~a; match-edge: ~a)~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':sprout)
          (getf entry ':topmost)
          (getf entry ':match-edge)))

(defmethod print-log-entry (entry
                            (source dryad)
                            (entry-type (eql ':processing-pair))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] processing match (~{~a~^ ~}) with ids (~{~a~^ ~})~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':address-pair)
          (getf entry ':id-pair)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':aborting-multireweight-collection))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] aborting ~a's MRW bc root collection failed for root-bucket (~{~a~^ ~})~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':source-root)
          (getf entry ':root-bucket)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':aborting-multireweight-solo))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] aborting ~a's MRW bc it's the only root in the cluster~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':source-root)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':aborting-multireweight-priority))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] aborting ~a (~a)'s MRW bc priority vs. hold-cluster (~{~a~^ ~}) w/ min-id ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':source-root)
          (getf entry ':source-id)
          (getf entry ':hold-cluster)
          (getf entry ':cluster-min-id)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':set-held-by-roots))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] setting held-by-roots of hold-cluster (~{~a~^ ~}) to itself~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':held-by-roots)))

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

(defmethod algorithmic-entry? (entry (source dryad))
  (member (getf entry ':entry-type) '(:handling-sow
                                      :dryad-sending-expand
                                      :processing-pair)))

(defmethod algorithmic-entry? (entry (source supervisor))
  (member (getf entry ':entry-type) '(:got-recommendation
                                      :success
                                      :reweighting
                                      :rewinding)))

(defmethod algorithmic-entry? (entry (source blossom-node))
  (member (getf entry ':entry-type) '(:set-up-blossom
                                      :blossom-extinguished)))

(defun reduced-log (&optional (logger *logger*))
  "Trims log messages to only ones of primary interest (see `ALGORITHMIC-ENTRY?')."
  (let (entries
        (successful-processes (successful-supervisors (logger-entries logger))))
    (dolist (entry (reverse (logger-entries logger)) (reverse entries))
      (let ((source (getf entry ':source)))
        (cond
          ;; dryad logs
          ((and (typep source 'dryad)
                (algorithmic-entry? entry source))
           (push entry entries))
          ;; supervisor logs
          ((and (typep source 'supervisor)
                (member source successful-processes)
                (algorithmic-entry? entry source))
           (push entry entries))
          ;; blossom logs
          ((and (typep source 'blossom-node)
                (algorithmic-entry? entry source))
           (push entry entries)))))))

(defun print-reduced-log (&optional (logger *logger*))
  "Shorthand for printing the results of `REDUCED-LOG'."
  (print-log (reduced-log logger)))

(defgeneric debug-entry? (entry source)
  (:documentation "Used to define which subset of `ENTRY' types emanating from `SOURCE' are helpful for debugging.")
  (:method (entry source) nil))

(defmethod debug-entry? (entry (source supervisor))
  (member (getf entry ':entry-type) '(:aborting-multireweight-collection
                                      :aborting-multireweight-priority
                                      :aborting-multireweight-solo
                                      :set-held-by-roots)))

(defun debug-log (&optional (logger *logger*))
  "Trims log messages to only ones useful to debugging (see `DEBUG-ENTRY?')."
  (let (entries
        (successful-processes (successful-supervisors (logger-entries logger))))
    (dolist (entry (reverse (logger-entries logger)) (reverse entries))
      (let ((source (getf entry ':source)))
        (cond
          ;; dryad logs
          ((and (typep source 'dryad)
                (or (algorithmic-entry? entry source)
                    (debug-entry? entry source)))
           (push entry entries))
          ;; supervisor logs
          ((and (typep source 'supervisor)
                (or (algorithmic-entry? entry source)
                    (debug-entry? entry source)))
           (push entry entries))
          ;; blossom logs
          ((and (typep source 'blossom-node)
                (or (algorithmic-entry? entry source)
                    (debug-entry? entry source)))
           (push entry entries)))))))

(defun print-debug-log (&optional (logger *logger*))
  "Shorthand for printing the results of `DEBUG-LOG'."
  (print-log (debug-log logger)))
