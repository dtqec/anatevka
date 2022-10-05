;;;; utilities.lisp
;;;;
;;;; Common, package-wide utilities.

(in-package #:anatevka)

(defun vnth (index array)
  "Like NTH, but for vectors."
  (aref array index))

(defun array-from-fn (length fn &rest array-initargs)
  "Initializes an array of a particular LENGTH (and with possible other ARRAY-INITARGS) by evaluating (FN J) for each position 0 <= J < LENGTH and storing the result at (AREF ARRAY J)."
  (initialize-and-return ((array (apply #'make-array length array-initargs)))
    (dotimes (j length)
      (setf (aref array j) (funcall fn j)))))

(defun discard-args (fn)
  "Converts NIL -> B' to A' -> B' by discarding the input."
  (lambda (&rest args)
    (declare (ignore args))
    (funcall fn)))

(defun pick-the-other (thing option1 option2)
  "Selects the \"other\" item from a pair."
  (cond
    ((eql thing option1)
     option2)
    ((eql thing option2)
     option1)
    (t
     (error "~a is neither ~a nor ~a" thing option1 option2))))

(defun latest-common-head (l r &key
                                 prev   ; XXX: could be more robust
                                 (test #'equalp)
                                 key) 
  "Given a pair of lists

    (A1 A2 ... An A(n+1) ... Am) and
    (A1 A2 ... An B(n+1) ... Bl),

computes the pair

    ((An A(n+1) ... Am) (An B(n+1) ... Bl))."
  (unless (and l r)
    ;; if we reach this unless block, it is because one of the tails
    ;; that constitutes the fresh blossom cycle entirely contains the
    ;; other one. but, we still need to hold onto the `prev` value
    (return-from latest-common-head
      (list (list* (car prev) l) (list* (cdr prev) r))))
  (let* ((fl (first l))
         (fr (first r))
         (tfl (if key (funcall key fl) fl))
         (tfr (if key (funcall key fr) fr)))
    (cond
      ((funcall test tfl tfr)
       (latest-common-head (rest l) (rest r)
                           :prev (cons fl fr)
                           :test test
                           :key key))
      (t
       (list (list* (car prev) l) (list* (cdr prev) r))))))

(defun -> (obj &rest slots)
  "Analogous to the C expression obj->slot1->slot2->...->slotn."
  (if slots
      (a:when-let ((obj-prime (slot-value obj (first slots))))
        (apply #'-> obj-prime (rest slots)))
      obj))

(defun (setf ->) (value obj &rest slots)
  "Analogous to the C expression obj->slot1->slot2->...->slotn = value."
  (cond
    ((endp slots)
     (setf obj value))
    ((endp (rest slots))
     (setf (slot-value obj (first slots)) value))
    (t
     (apply #'(setf ->) value (slot-value obj (first slots)) (rest slots)))))

(defmacro with-with ((&rest with-designators) &body body)
  "Permits the chaining of context macros via WITH-DESIGNATORS."
  (loop :with output := `(progn ,@body)
        :for (head args) :in (reverse with-designators)
        :do (setf output `(,head ,args ,output))
        :finally (return output)))

(defun make-bool (val)
  "Turns a generalized boolean into a boolean."
  (not (not val)))

(defun pick-randomly (list)
  "Picks uniformly randomly from a noncircular list."
  (nth (random (length list)) list))
