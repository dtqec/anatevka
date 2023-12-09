;;;; anatevka-tests.asd

(asdf:defsystem #:anatevka-tests
  :description "Regression tests for Anatevka."
  :author "Eric Peterson <peterson.eric.c@gmail.com>, Peter Karalekas <peter@karalekas.com>"
  :depends-on (#:anatevka
               #:fiasco
               #:uiop
               #:closer-mop
               #:trivial-garbage
              )
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':anatevka-tests
                                           '#:run-anatevka-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "node")
               (:file "dryad")
               (:module "operations"
                :serial t
                :components ((:file "graft")
                             (:file "augment")
                             (:file "expand")
                             (:file "contract")
                             (:file "multireweight")
                             (:file "reweight")))
               (:file "blossom")))
