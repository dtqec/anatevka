;;;; logger.lisp
;;;;
;;;; Structured logging and log processing.

(in-package #:anatevka)

;;;
;;; pretty-printing mechanisms
;;;

(defmethod print-log-entry (entry
                            (source-type (eql 'SUPERVISOR))
                            (entry-type (eql 'GOT-RECOMMENDATION))
                            &optional (stream *standard-output*))
  (format stream "~5f: SUPERVISOR ~a got recommendation ~a (~a; ~{~a~^ ~}) from root: ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (getf entry ':recommendation)
          (getf entry ':weight)
          (getf entry ':edges)
          (getf entry ':source-root)))

(defmethod print-log-entry (entry
                            (source-type (eql 'SUPERVISOR))
                            (entry-type (eql 'SUCCESS))
                            &optional (stream *standard-output*))
  (format stream "~5f: SUPERVISOR ~a closing.~%"
          (getf entry ':time) (getf entry ':source)))

(defmethod print-log-entry (entry
                            (source-type (eql 'BLOSSOM-NODE))
                            (entry-type (eql 'SET-UP-BLOSSOM))
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

(defmethod print-log-entry (entry
                            (source-type (eql 'DRYAD))
                            (entry-type (eql 'HANDLING-SOW))
                            &optional (stream *standard-output*))
  (format stream "~5f: Spawning blossom ~a at ~a.~%"
          (getf entry ':time)
          (getf entry ':address)
          (getf entry ':id)))

;;;
;;; filtering routines
;;;

(defun successful-supervisors (entries)
  "Collects addresses of supervisors which either complete successfully or fail to complete at all."
  (loop :for entry :in entries
        :when (and (eql 'SUPERVISOR (getf entry ':source-type))
                   (eql 'SUCCESS (getf entry ':entry-type))
                   (eql T (getf entry ':success)))
          :collect (getf entry ':source) :into positive-addresses
        :when (and (eql 'SUPERVISOR (getf entry ':source-type))
                   (eql 'GOT-RECOMMENDATION (getf entry ':entry-type))
                   (eql ':HOLD (getf entry ':recommendation))
                   (address=
                    (blossom-edge-source-node (first (getf entry ':edges)))
                    (blossom-edge-target-node (first (getf entry ':edges)))))
          :collect (getf entry ':source) :into self-held-addresses
        :when (and (eql 'SUPERVISOR (getf entry ':source-type))
                   (eql 'COMMAND (getf entry ':entry-type))
                   (eql ':START (getf entry ':command)))
          :collect (getf entry ':source) :into start-addresses
        :when (and (eql 'SUPERVISOR (getf entry ':source-type))
                   (eql 'SUCCESS (getf entry ':entry-type)))
          :collect (getf entry ':source) :into done-addresses
        :finally (return (union (set-difference positive-addresses
                                                self-held-addresses
                                                :test #'address=)
                                (set-difference start-addresses
                                                done-addresses
                                                :test #'address=)))))

(defun reduce-log (log)
  "Trims log messages to only ones of primary interest."
  (let (entries
        (successful-addresses (successful-supervisors (logger-entries log))))
    (dolist (entry (reverse (logger-entries log)) (reverse entries))
      (cond
        ((and (eql 'SUPERVISOR (getf entry ':source-type))
              (eql 'GOT-RECOMMENDATION (getf entry ':entry-type))
              (member (getf entry ':source) successful-addresses :test #'address=))
         (push entry entries))
        ((or (and (eql 'SUPERVISOR (getf entry ':source-type))
                  (eql 'SUCCESS (getf entry ':entry-type))
                  (member (getf entry ':source) successful-addresses :test #'address=))
             (and (eql 'SUPERVISOR (getf entry ':source-type))
                  (eql 'REWINDING (getf entry ':entry-type)))
             (and (eql 'SUPERVISOR (getf entry ':source-type))
                  (eql 'MULTIREWEIGHTING (getf entry ':entry-type)))
             (and (eql 'MESSAGE-WILT (type-of (getf entry ':payload))))
             (and (eql 'SPAWNED-FRESH-BLOSSOM (getf entry ':entry-type)))
             (and (eql 'BLOSSOM-EXTINGUISHED (getf entry ':entry-type))))
         (push entry entries))
        ;; dryad logs
        ((and (eql 'DRYAD (getf entry ':source-type))
              ;; dryad sowing
              (or (eql 'HANDLING-SOW (getf entry ':entry-type))
                  (eql 'HANDLING-SPROUT (getf entry ':entry-type))
                  (eql 'PROCESSING-PAIR (getf entry ':entry-type))
                  ;; dryad expansion
                  (and (eql 'COMMAND (getf entry ':entry-type))
                       (eql 'SEND-EXPAND (getf entry ':command)))
                  (eql 'DRYAD-SENDING-EXPAND (getf entry ':entry-type))))
         (push entry entries))))))
