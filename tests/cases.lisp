;;;; tests/cases.lisp
;;;;
;;;; Runs the distributed blossom algorithm against a family of graphs with known
;;;; MWPM, and asserts that we find the correct answer.

(in-package :anatevka-tests)

(eval-when (:compile-toplevel :load-toplevel :execute) ; needed at compile-time

  (defun test-case-directory-filepath (dirname system-name)
    "Return the absolute pathname for `DIRNAME', which is the name of a test case directory in the `tests/py/cases'."
    (declare (string dirname))
    (let* ((system-dir (ql:where-is-system system-name)))
      (merge-pathnames (concatenate 'string "tests/py/cases/" dirname "/*.*")
                       system-dir)))

  (defun parse-test-case-file (filename)
    "Parses a test case file and extracts the MWPM value and the list of nodes."
    (with-open-file (stream filename :direction :input)
      (let ((mwpm-value (parse-integer (read-line stream))) ; read first line for mwpm
            (nodes '()))
        (loop :for line := (read-line stream nil)
              :while line
              :do (destructuring-bind (x y)
                     (mapcar #'parse-integer (uiop:split-string line :separator ","))
                   (push (list x y) nodes)))
        (values mwpm-value (reverse nodes)))))

  (defun process-test-case-directory (dirname system-name parser-fn)
    "Iterates through all files in test case directory named `DIRNAME' and parses them."
    (let* ((dirpath (test-case-directory-filepath dirname system-name))
           (files (directory dirpath)))
      (loop :for file :in files
            :collect (list file (multiple-value-list (funcall parser-fn file)))
              :into test-cases
            :finally (return test-cases))))

  (defun sanitize-test-name (dirname test-path suite-name package-keyword)
    "Generates a valid Lisp symbol for a test name based on directory and file name."
    (let* ((test-name (pathname-name test-path))
           (clean-name (format nil "test-~A-suite-~A-~A" suite-name dirname test-name)))
      (intern (string-upcase (substitute #\- #\/ clean-name)) package-keyword))))

(defmacro define-blossom-suite (dirname
                                (&key (border +default-border+)
                                      (debug? nil)
                                      (dryad-clock-rate +default-dryad-clock-rate+)
                                      (iterations +default-iterations+)
                                      (timeout +default-timeout+)
                                      (timestep +default-timestep+)
                                      (dryad-class 'dryad)))
  "Creates a suite of `DEFINE-BLOSSOM-TEST's by processing the test case files at `DIRNAME' and asserting that we produce the correct minimum-weight perfect matching (MWPM). The `DIRNAME' is a number n specifying a directory containing test cases of random complete graphs laid out on an nxn grid. The test names produced by `DEFINE-BLOSSOM-SUITE' are of the form `TEST-BLOSSOM-SUITE-n-p-i' where p is the 'node density' (i.e. what proportion of the grid locations have a node at them) and i is the test case number."
  `(progn
     ,@(loop :for (test-path (mwpm-value nodes))
               :in (process-test-case-directory dirname
                                                "anatevka-tests"
                                                #'parse-test-case-file)
             :for test-name := (sanitize-test-name dirname
                                                   test-path
                                                   "blossom"
                                                   :anatevka-tests)
             :collect
             `(define-blossom-test ,test-name ,nodes
                  (:border ,border :debug? ,debug? :dryad-clock-rate ,dryad-clock-rate
                   :iterations ,iterations :timeout ,timeout :timestep ,timestep
                   :dryad-class ',dryad-class :solution-weight ,mwpm-value)
                ()))))

(define-blossom-suite "10" ()) ; 10x10 grid tests

(define-blossom-suite "20" ()) ; 20x20 grid tests
