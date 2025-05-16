;;;; anatevka.asd
;;;;
;;;; Author: Eric Peterson, Peter Karalekas

(asdf:defsystem #:anatevka
  :description "A distributed blossom algorithm for minimum-weight perfect matching."
  :author "Eric Peterson <peterson.eric.c@gmail.com>, Peter Karalekas <peter@karalekas.com>"
  :version (:read-file-form "VERSION.txt")
  :pathname "src/"
  :depends-on (#:alexandria
               (:version #:aether "1.2.0")
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:anatevka-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl(sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "dryad-api")
               (:file "node")
               (:file "supervisor")
               (:file "lock")
               (:module "operations"
                :serial t
                :components ((:file "scan")
                             (:file "graft")
                             (:file "augment")
                             (:file "expand")
                             (:file "contract")
                             (:file "multireweight")
                             (:file "reweight")))
               (:file "dryad")
               (:file "logger")))
