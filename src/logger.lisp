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
                            (source blossom-node)
                            (entry-type (eql ':pinging-vertices))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] pinging vertices ~{~a~^ ~}~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':vertices)))

(defmethod print-log-entry (entry
                            (source blossom-node)
                            (entry-type (eql ':processing-reply-pong))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] processing reply pong ~a ~a ~{~a~^ ~} from ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':reply-pong-rec)
          (getf entry ':reply-pong-weight)
          (getf entry ':reply-pong-edges)
          (getf entry ':reply-pong-source)))

(defmethod print-log-entry (entry
                            (source blossom-node)
                            (entry-type (eql ':unified-reply-pong))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] unified pong is ~a ~a ~{~a~^ ~} from ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':unified-pong-rec)
          (getf entry ':unified-pong-weight)
          (getf entry ':unified-pong-edges)
          (getf entry ':unified-pong-source)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':got-recommendation))
                            &optional (stream *standard-output*))
  (if (eql ':hold (getf entry ':recommendation))
      (format stream "~5f: [~a] got ~a ~a ~{~a~^ ~} from ~a w/ root-bucket (~{~a~^ ~})~%"
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
                            (entry-type (eql ':checking-reweight))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] checking if roots (~{~a~^ ~}) are clear to reweight by ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':roots)
          (getf entry ':weight)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':check-reweight-details))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] checked if roots (~{~a~^ ~}) are clear to reweight by ~a and got pong ~a ~a ~{~a~^ ~} from ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':roots)
          (getf entry ':weight)
          (getf entry ':check-pong-rec)
          (getf entry ':check-pong-weight)
          (getf entry ':check-pong-edges)
          (getf entry ':check-pong-source)))

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
                            (entry-type (eql ':reweighting-finished))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] finished reweighting roots (~{~a~^ ~}) by ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':roots)
          (getf entry ':weight)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':checking-rewinding))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] checking if roots (~{~a~^ ~}) need to rewind~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':roots)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':check-rewinding-details))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] checked if roots (~{~a~^ ~}) need to rewind and got pong ~a ~a ~{~a~^ ~} from ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':roots)
          (getf entry ':rewinding-pong-rec)
          (getf entry ':minimum-weight-edge)
          (getf entry ':rewinding-pong-edges)
          (getf entry ':rewinding-pong-source)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':rewinding))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] rewinding roots (~{~a~^ ~}) by ~a (overall: ~a)~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':roots)
          (getf entry ':amount)
          (getf entry ':overall)))

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
                            (entry-type (eql ':aborting-multireweight-claim-failure))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] aborting MRW bc claiming failed for roots (~{~a~^ ~})~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':roots)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':multireweight-broadcast-scan-result))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] MRW cluster scan returned pong (~a ~a ~{~a~^ ~} source-root ~a source-id ~a target-root ~a) for hold-cluster (~{~a~^ ~})~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':recommendation)
          (getf entry ':weight)
          (getf entry ':edges)
          (getf entry ':source-root)
          (getf entry ':source-id)
          (getf entry ':target-root)
          (getf entry ':hold-cluster)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':aborting-multireweight-negative-pong))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] aborting MRW bc the cluster scan returned a negative-weight pong (~a ~a ~{~a~^ ~} source-root ~a source-id ~a target-root ~a) for hold-cluster (~{~a~^ ~})~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':recommendation)
          (getf entry ':weight)
          (getf entry ':edges)
          (getf entry ':source-root)
          (getf entry ':source-id)
          (getf entry ':target-root)
          (getf entry ':hold-cluster)))

(defmethod print-log-entry (entry
                            (source supervisor)
                            (entry-type (eql ':aborting-multireweight-zero-pong))
                            &optional (stream *standard-output*))
  (format stream "~5f: [~a] aborting MRW bc the cluster scan returned a zero-weight pong (~a ~a ~{~a~^ ~} source-root ~a source-id ~a target-root ~a) for hold-cluster (~{~a~^ ~})~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':recommendation)
          (getf entry ':weight)
          (getf entry ':edges)
          (getf entry ':source-root)
          (getf entry ':source-id)
          (getf entry ':target-root)
          (getf entry ':hold-cluster)))

;;;
;;; filtering routines
;;;

(defun supervisor-logs-for-address (address &optional (entries (logger-entries *logger*)))
  "Trims log `ENTRIES' to only ones related to SUPERVISOR actions involving `ADDRESS'."
  (let (trimmed-entries relevant-supervisors)
    (dolist (entry entries (reverse trimmed-entries))
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
         (push entry trimmed-entries))))))

;; TODO: should set-difference positive-processes and fully-rewound-processes
(defun successful-supervisors (&optional (entries (logger-entries *logger*)))
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

(defun remove-unsuccessful-supervisors (&optional (entries (logger-entries *logger*)))
  "Trims log `ENTRIES' by removing entries from unsuccessful SUPERVISORs."
  (let (trimmed-entries
        (successful-processes (successful-supervisors entries)))
    (dolist (entry entries (reverse trimmed-entries))
      (let ((source (getf entry ':source)))
        (unless (and (typep source 'supervisor)
                     (not (member source successful-processes)))
          (push entry trimmed-entries))))))

;;
;; printer functions
;;

(defun print-reduced-log (&key (entries (logger-entries *logger*))
                               (stream *standard-output*)
                               (start-time nil)
                               (end-time nil)
                               (log-level 2))
  "Shorthand for calling `PRINT-LOG' without unsuccessful supervisors. By default, this sets `LOG-LEVEL' to 2, which means we will only print algorithmically-relevant log entries. However, this can be changed to 1 to add INFO-like logs, or 0 to print everything. In addition, we can optionally provide a `START-TIME' and/or `END-TIME' to further trim the entries."
  (print-log :entries (remove-unsuccessful-supervisors entries)
             :stream stream
             :start-time start-time
             :end-time end-time
             :log-level log-level))
