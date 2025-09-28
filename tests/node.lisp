;;;; tests/matchmaker.lisp

(in-package #:anatevka-tests)

;;;
;;; utility data structure
;;;

(defstruct grid-location
  "A plane coordinate."
  x
  y)

(defmethod print-object ((object grid-location) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "(~a, ~a)"
            (grid-location-x object)
            (grid-location-y object))))

(defmethod min-id ((x grid-location) (y grid-location))
  (cond
    ((< (grid-location-x x) (grid-location-x y))
     x)
    ((and (= (grid-location-x x) (grid-location-x y))
          (< (grid-location-y x) (grid-location-y y)))
     x)
    (t
     y)))

(defmethod vertex-vertex-distance ((space-v grid-location) (space-w grid-location))
  (+ (abs (- (grid-location-x space-v) (grid-location-x space-w)))
     (abs (- (grid-location-y space-v) (grid-location-y space-w)))))

;;;
;;; utility tests
;;;

(deftest test-unify-pongs ()
  (let* ((augment-0 (anatevka::make-message-pong
                     :weight 0
                     :recommendation ':augment))
         (augment-neg (anatevka::make-message-pong
                       :weight -1
                       :recommendation ':augment))
         (contract-0 (anatevka::make-message-pong
                      :weight 0
                      :recommendation ':contract))
         (hold-0 (anatevka::make-message-pong
                  :weight 0
                  :recommendation ':hold))
         (hold-pos (anatevka::make-message-pong
                    :weight 1
                    :recommendation ':hold))
         (graft-0 (anatevka::make-message-pong
                   :weight 0
                   :recommendation ':graft))
         (zero-weighters (list augment-0 graft-0 contract-0)))
    ;; non-zero HOLDs have higher precedence than equal-weight ops
    (is (every #'identity
               (loop :for rec :in (list ':augment :graft ':expand ':contract)
                     :collect (let ((pong (anatevka::make-message-pong
                                           :weight (anatevka::message-pong-weight
                                                    hold-pos)
                                           :recommendation rec)))
                                (eql ':hold
                                     (anatevka::message-pong-recommendation
                                      (anatevka::unify-pongs hold-pos pong)))))))
    ;; HOLD 0 has lower precedence than other zero-weight ops
    (is (every #'identity
               (loop :for pong :in zero-weighters
                     :collect (not (eql ':hold
                                        (anatevka::message-pong-recommendation
                                         (anatevka::unify-pongs hold-0 pong)))))))
    ;; negative-weight operations (other than PASS, internal HOLD) take precedence
    (is (every #'identity
               (loop :for pong :in zero-weighters
                     :collect (= -1
                                 (anatevka::message-pong-weight
                                  (anatevka::unify-pongs augment-neg pong))))))))

(deftest test-find-even-arm ()
  (let ((test-cases '((;; source-index - target-index is even, positive
                       (5 3 (1 2 3 4 5 6 7)) .
                       ((5 4) T (3 2 1 7 6 5 4)))
                      (;; source-index - target-index is even, negative
                       (3 5 (1 2 3 4 5 6 7)) .
                       ((3 4) NIL (5 6 7 1 2 3 4)))
                      (;; source-index - target-index is odd, positive
                       (5 2 (1 2 3 4 5 6 7)) .
                       ((5 6 7 1) NIL (2 3 4 5 6 7 1)))
                      (;; source-index - target-index is odd, negative
                       (2 5 (1 2 3 4 5 6 7)) .
                       ((2 1 7 6) T (5 4 3 2 1 7 6)))
                      (;; edge case
                       (3 3 (1 2 3 4 5 6 7)) .
                       (() NIL (3 4 5 6 7 1 2))))))
    (loop :for ((source target list) . (arm reversed? full)) :in test-cases
          :do (multiple-value-bind (real-arm really-reversed? real-full)
                  (anatevka::find-even-arm list source target)
                (is (equalp real-arm arm))
                (is (eql reversed? really-reversed?))
                (is (equalp full real-full))))))

(deftest test-find-even-arm-reversal ()
  "Test that the `:key' and `:rev' keywords do what they are supposed to."
  (let ((cycle '((1 2) (2 3) (3 4) (4 5) (5 1)))
        (path '((2 1) (1 5) (5 4) (4 3)))
        (reversed? T)
        (full-path '((3 2) (2 1) (1 5) (5 4) (4 3))))
    (multiple-value-bind (real-path real-reversed? real-full-path)
        (anatevka::find-even-arm cycle 2 3
                                 :key #'car
                                 :rev (lambda (x)
                                        (mapcar #'reverse (reverse x))))
      (is (equalp real-path path))
      (is (equalp real-reversed? reversed?))
      (is (equalp real-full-path full-path)))))

;;;
;;; a fixed test of the blossom algorithm outside of the context of a dryad
;;;

(defstruct mma-id
  "A wrapper for a vertex ID used in the Mathematica blossom demo."
  (value nil :type (integer 1 8)))

(defmethod anatevka::vertex-vertex-distance ((id-v mma-id) (id-w mma-id))
  (let ((v (mma-id-value id-v))
        (w (mma-id-value id-w)))
    (aref #2A(( 0 40 52 50 46 70 36 46)
              (40  0 34 54 28 64 20  6)
              (52 34  0 28 34 24  2 30)
              (50 54 28  0 42 18 36  8)
              (46 28 34 42  0 14 80 22)
              (70 64 24 18 14  0 22 64)
              (36 20  2 36 80 22  0 80)
              (46  6 30  8 22 64 80  0))
          (1- v) (1- w))))
;; NOTE: a perfect matching for this table has weight 64:
;;       1 -- 2, 3 -- 7, 4 -- 8, 5 -- 6

(deftest test-weighted-blossom-results-against-mathematica ()
  "This integration test duplicates the input used by the Mathematica demo of the weighted blossom algorithm for 8 vertices and random seed 4, then applies our own algorithm."
  (let* ((*local-courier* (make-courier :processing-clock-rate 300))
         (match-address (register))
         (dryad (spawn-process 'dryad
                               :process-clock-rate 20
                               :debug? t
                               :match-address match-address))
         (simulation (make-simulation)))
    (simulation-add-event simulation
                          (make-event :callback *local-courier* :time 0))
    (simulation-add-event simulation (make-event :callback dryad :time 0))
    ;; set up simulation components
    (with-active-simulation simulation
      (loop :for j :from 1 :to 8
            :for id := (make-mma-id :value j)
            :do (send-message (process-public-address dryad)
                              (anatevka::make-message-sow :id id))))
    ;; run simulation until maximally matched (i.e., the dryad terminates)
    (simulation-run simulation :canary (canary-process dryad))
    (labels ((drain-match-address (&optional acc)
               (receive-message (match-address message)
                 (message-reap
                  (drain-match-address (list* (message-reap-ids message) acc)))
                 (otherwise
                  acc))))
      
      ;; make sure the matching is of the predicted minimal weight
      (is (= 64 (loop :for (left right) :in (drain-match-address)
                      :sum (anatevka::vertex-vertex-distance left right)))))))

;;;
;;; macrology for setting up blossom state
;;;

(defmacro blossom-let ((tree-name &rest global-options)
                       (&rest node-definitions)
                       &body body)
  "Helper macro for defining a family of `BLOSSOM-NODE's to be organized into a tree.  Each entry of `NODE-DEFINITIONS' is a `LET'-like binding:

    (NODE-NAME OPTIONAL-NODE-CLASS . KEYWORD ARGUMENTS FOR MAKE-INSTANCE 'NODE-CLASS) ,

where NODE-CLASS is replaced by OPTIONAL-NODE-CLASS if supplied or by BLOSSOM-NODE if omitted.  Additionally, GLOBAL-OPTIONS are appended to the keyword arguments passed to MAKE-INSTANCE.

Finally, all of the nodes constructed by this BLOSSOM-LET are stashed in the place indicated by TREE-NAME.  This is convenient for use with `TREE-EQUALP' and with `SIMULATE-ADD-TREE'."
  (let (addresses augmented-node-definitions)
    (loop :for (symbol-name . rest) :in node-definitions
          :for class-name := (if (keywordp (first rest))
                                 'blossom-node
                                 (first rest))
          :for chopped-args := (if (keywordp (first rest))
                                   rest
                                   (rest rest))
          ;; precompute addresses for the nodes.
          ;; if the node arguments include :process-courier, use the provided
          ;; courier instance when pre-registering the address for this node
          :for courier := (getf chopped-args :process-courier)
          :for courier-clause := (when courier `(:courier ,courier))
          :collect `(,symbol-name (register
                                   ,@courier-clause
                                   :channel ',(gensym
                                               (format nil "TEST-BLOSSOM-~A"
                                                       symbol-name))))
            :into addresses-result
          ;; then, set up definitions from clauses
          :collect `(,symbol-name (spawn-process ',class-name
                                                 :process-key ,symbol-name
                                                 ,@chopped-args
                                                 ,@global-options
                                                 :debug? t))
            :into definitions-result
          :finally (setf addresses addresses-result
                         augmented-node-definitions definitions-result))
    `(let ,addresses
       (declare (ignorable ,@(mapcar #'first node-definitions)))
       (let ,augmented-node-definitions
         (let ((,tree-name (list ,@(mapcar #'first node-definitions))))
           (setf ,@(loop :for (name . rest) :in node-definitions
                         :nconc (list `(process-command-stack ,name)
                                      '(list '(anatevka::IDLE)))))
           ,@body)))))

(defun simulate-add-tree (simulation tree &key (start-time 0) dryad (sprouted? t))
  "BLOSSOM-LET binds a place that knows about all the nodes it's constructed, called the \"tree\". SIMULATE-ADD-TREE takes the contents of this place and adds it to a SIMULATION object. If a `DRYAD' is provided, we set its slots for each node."
  (dolist (node tree)
    (simulation-add-event simulation (make-event :callback node :time start-time))
    (when dryad
      (unless (anatevka::blossom-node-petals node)
        (let ((id (slot-value node 'anatevka::id))
              (address (process-public-address node)))
          (setf (gethash address (dryad-ids dryad))        id
                (gethash address (dryad-sprouted? dryad))  sprouted?))))))

(defun simulate-until-dead (simulation process &key (start-time 0) timeout)
  "Runs SIMULATION until PROCESS exhausts its command queue."
  ;; NOTE: Rather than waiting on the command queue, we could check whether the
  ;; process's public address is still registered (cf., the PROCESS-DIE handler
  ;; in DEFINE-OBJECT-HANDLER PROCESS).
  (simulation-run simulation
                  :canary (funcall #'canary-any
                                   (canary-process process)
                                   (if timeout
                                       (canary-until timeout)
                                       (constantly nil))))
  (values simulation (aether::simulation-horizon simulation)))

(defun tree-equalp (left-nodes right-nodes)
  "Checks that two trees of BLOSSOM-NODEs agree structurally, up to renaming of addresses.  Expects to receive the nodes of the two trees in the same order in both arguments."
  (let ((dictionary (make-hash-table :hash-function #'anatevka::hash-address :test #'address=)))
    (labels ((translated-edge= (l r)
               (let ((accessors (list #'anatevka::blossom-edge-source-node
                                      #'anatevka::blossom-edge-source-vertex
                                      #'anatevka::blossom-edge-target-vertex
                                      #'anatevka::blossom-edge-target-node)))
                 (every #'address=
                        (mapcar (lambda (f) (gethash (funcall f l) dictionary))
                                accessors)
                        (mapcar (lambda (f) (funcall f r)) accessors))))
             (address-equalp (l r)
               (address= (gethash l dictionary l) r))
             (test (left-value right-value)
               (typecase left-value
                 (list
                  (and (listp right-value)
                       (= (length left-value) (length right-value))
                       (or (endp left-value)
                           (and (typep (first left-value) 'anatevka::blossom-edge)
                                (every #'translated-edge= left-value right-value))
                           (and (typep (first left-value) 'aether::address)
                                (every #'address-equalp left-value right-value)))))
                 (anatevka::blossom-edge
                  (and (typep right-value 'anatevka::blossom-edge)
                       (translated-edge= left-value right-value)))
                 (anatevka::address
                  (address-equalp left-value right-value))
                 (otherwise
                  (equalp left-value right-value))))
             (blossom-slots (value)
               (initialize-and-return ((slots nil))
                 (block nil
                   (dolist (superclass (closer-mop:class-precedence-list (class-of value)))
                     (dolist (slot-definition (closer-mop:class-direct-slots superclass))
                       (push (closer-mop:slot-definition-name slot-definition) slots))
                     (when (equalp superclass (closer-mop:ensure-class
                                               'anatevka:blossom-node))
                       (return)))))))
      (loop :for left :in left-nodes
            :for right :in right-nodes
            :do (setf (gethash (process-public-address left) dictionary)
                      (process-public-address right)))
      (anatevka::initialize-and-return ((test-result t))
        (loop :for left :in left-nodes
              :for right :in right-nodes
              :do (unless (equalp (class-of left) (class-of right))
                    (format t "~&left (~a) class: ~a~%right (~a) class: ~a~%"
                            left (class-of left)
                            right (class-of right))
                    (setf test-result nil))
                  (loop :for slot :in (blossom-slots left)
                        :for left-value := (slot-value left slot)
                        :for right-value := (slot-value right slot)
                        :unless (test left-value right-value)
                          :do (format t "~&left (~a) ~a: ~a~%right (~a) ~a: ~a~%"
                                      left slot left-value
                                      right slot right-value)
                              (setf test-result nil)))))))

(defun vv-edge (source-vertex target-vertex)
  "Helper constructor for making a BLOSSOM-EDGE between two vertices."
  (anatevka::make-blossom-edge
   :source-vertex (if (typep source-vertex 'address)
                      source-vertex
                      (process-public-address source-vertex))
   :target-vertex (if (typep target-vertex 'address)
                      target-vertex
                      (process-public-address target-vertex))))

(defun bb-edge (source-node source-vertex target-vertex target-node)
  "Helper constructor for making a BLOSSOM-EDGE between two nodes of the form:

   source-node:source-vertex -> target-vertex:target-node
"
  (make-blossom-edge
   :source-node (if (typep source-node 'address)
                    source-node
                    (process-public-address source-node))
   :source-vertex (if (typep source-vertex 'address)
                      source-vertex
                      (process-public-address source-vertex))
   ;; these can be null in an expand recommendation
   :target-vertex (if (or (typep target-vertex 'address)
                          (null target-vertex))
                      target-vertex
                      (process-public-address target-vertex))
   :target-node (if (or (typep target-node 'address)
                        (null target-node))
                    target-node
                    (process-public-address target-node))))

(defun supervisor (simulation &rest pong-initargs &key (time 0) &allow-other-keys)
  "Helper constructor for a SUPERVISOR primed to run a bespoke PONG."
  (initialize-and-return
      ((supervisor (spawn-process 'anatevka::supervisor :debug? t)))
    (simulation-add-event simulation (make-event :callback supervisor :time time))
    (remf pong-initargs :time)
    (push (apply #'anatevka::make-message-pong
                 (append pong-initargs
                         (list :weight 0)))
          (process-data-stack supervisor))))

(defun id (&optional
             (x 0)
             (y 0))
  "Helper constructor for throwaway IDs."
  (make-grid-location :x x :y y))
