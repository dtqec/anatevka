;;;; tests/suite.lisp

(in-package #:anatevka-tests)

(defun run-anatevka-tests (&key (verbose nil) (headless nil))
  "Run all Anatevka tests. If VERBOSE is T, print out lots of test info. If HEADLESS is T, disable interactive debugging and quit on completion."
  ;; Bug in Fiasco commit fe89c0e924c22c667cc11c6fc6e79419fc7c1a8b
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream *standard-output*))
  (cond
    ((null headless)
     (run-package-tests :package ':anatevka-tests
                        :verbose verbose
                        :describe-failures t
                        :interactive t))
    (t
     (let ((successp (run-package-tests :package ':anatevka-tests
                                        :verbose t
                                        :describe-failures t
                                        :interactive nil)))
       (uiop:quit (if successp 0 1))))))
