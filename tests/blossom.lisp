;;;; tests/blossom.lisp
;;;;
;;;; This file implements a framework for testing the correctness of the blossom
;;;; algorithm. The heart of the framework is the macro `DEFINE-BLOSSOM-TEST',
;;;; which takes a problem graph and collection of minimum-weight perfect
;;;; matchings (MWPMs) that are valid solutions to the problem, and builds a
;;;; unit test on top of fiasco's DEFTEST framework.
;;;;
;;;; To aid in understanding the various unit tests, each one has a docstring
;;;; that shows the problem graph, and the various solutions. An example of this
;;;; is the following, with the problem graph on the left, separated from the
;;;; solutions by the => symbol (which are themselves separated by || symbols).
;;;; To more clearly differentiate between the different edges that compose each
;;;; matching, we use o, O, and * to each represent nodes of different matches.
;;;; For some graphs, we also use the + symbol as a visual aid for intersecting
;;;; edges and 'corners' of edges.
;;;;
;;;;                 o - o      o - o      o   O      o - o
;;;;                 |   |                 |   |
;;;;                 o - o  =>  O   *  ||  o   O  ||  O - O
;;;;                 |   |      |   |
;;;;                 o - o      O   *      * - *      * - *
;;;;
;;;; To make test-writing as natural as possible, problem graph coordinates are
;;;; laid out on a standard Manhattan/taxicab lattice (i.e. square lattice with
;;;; one unit between each node).
;;;;
;;;; NOTE: This is the only file in anatevka where we refer to concepts in
;;;;       quantum error correction. This testing framework is used heavily in
;;;;       the sequel package to anatevka where we build a distributed online
;;;;       decoder for the surface code, and we haven't yet gone through the
;;;;       effort of generalizing it.

(in-package #:anatevka-tests)

(defconstant +default-border+ 0)
(defconstant +default-dryad-clock-rate+ 10)
(defconstant +default-iterations+ 10)
(defconstant +default-timeout+ 1000)
(defconstant +default-timestep+ 5)

;; these utilities get used in the macro code which defines this part of the
;; test suite, so have to be available at compile-time.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun manhattan-distance (a b)
    (loop :for aa :in a
          :for bb :in b
          :sum (abs (- aa bb))))
  (defun match-weight (match)
    (apply #'manhattan-distance match))
  (defun matching-weight (matching)
    (reduce '+ (mapcar #'match-weight matching))))

(defun shift-coordinates (x y offset)
  "Given INTEGER coordinates `X' and `Y', apply an INTEGER `OFFSET' to `X', and then shift them to the 'surface code coordinate system' of the surface code. In the surface code, each flavor of stabilizer occupies its own lattice (thus we will only care about one of the two for the blossom algorithm). One such lattice has 'origin' at (1, 0), and every other stabilizer is 2 units away in the x or y direction."
  (list (1+ (* 2 (+ x offset))) (* 2 y)))

(defun sow-spacelike-graph (dryad coordinates
                            &key offset)
  "Given a LIST of spacelike `COORDINATES', sow them into the `DRYAD'. Before sowing, apply the INTEGER `OFFSET' and shift the coordinates to the surface code coordinate system using `SHIFT-COORDINATES'."
  (loop :for (x y) :in coordinates
        ;; apply offset and shift to 'lattice coordinate system'
        :for (shifted-x shifted-y) := (shift-coordinates x y offset)
        :for id := (make-grid-location :x shifted-x :y shifted-y)
        :do (send-message (process-public-address dryad)
                          (make-message-sow :id id))))

(defun coordinate< (a b)
  "Determines if coordinate `A' should go before coordinate `B', where both are LISTs in the form (X0 Y0) where X0 and Y0 are both INTEGERs. First compares by closeness to origin, and then by X0."
  (let* ((time-a (if (caddr a) (caddr a) 0))
         (time-b (if (caddr b) (caddr b) 0))
         (distance-to-origin-a (+ (car a) (cadr a)))
         (distance-to-origin-b (+ (car b) (cadr b))))
    (cond
      ((< time-a time-b)
       t)
      ;; if you're closer to origin, you come first
      ((and (= time-a time-b)
            (< distance-to-origin-a distance-to-origin-b))
       t)
      ;; break ties by comparing X0
      ((and (= time-a time-b)
            (= distance-to-origin-a distance-to-origin-b))
       (< (car a) (car b)))
      (t
       nil))))

(defun sort-match (match)
  "Given a `MATCH', which is a LIST of the form ((X0 Y0) (X1 Y1)) where X0, Y0, X1, and Y1 are INTEGERs, sort its two coordinates using the `COORDINATE<' predicate."
  (sort match #'coordinate<))

(defun unshift-coordinates (x y offset)
  "The opposite action of SHIFT-COORDINATES -- bring us back to the original Manhattan/taxicab lattice."
  (check-type x integer)
  (check-type y integer)
  (check-type offset integer)
  (assert (oddp x))
  (assert (evenp y))
  (list (- (/ (1- x) 2) offset) (/ y 2)))

(defun unpack-match (reap-message offset)
  "Given a message `REAP-MESSAGE' of type message-reap, unpack the match it contains as a LIST, unshifting the coordinates from the surface code coordinate system, and unapplying the `OFFSET'."
  (destructuring-bind (id-a id-b) (anatevka::message-reap-ids reap-message)
    (let* ((location-a id-a)
           (location-b id-b)
           (a-x (grid-location-x location-a))
           (a-y (grid-location-y location-a))
           (b-x (grid-location-x location-b))
           (b-y (grid-location-y location-b))
           (match (list (unshift-coordinates a-x a-y offset)
                        (unshift-coordinates b-x b-y offset))))
      (sort-match match))))

(defun contains-coordinate? (matching coordinate)
  "Returns T if the `MATCHING', which is a LIST in the form ((X0 Y0) (X1 Y1)) where X0, Y0, X1, Y1 are all INTEGERs, contains the `COORDINATE', which is a LIST of INTEGERs in the form (X0 Y0)."
  (flet ((match-contains-coordinate? (match)
           (or (equal coordinate (car match))
               (equal coordinate (cadr match)))))
    (some #'identity (mapcar #'match-contains-coordinate? matching))))

(defun perfect-matching? (matching coordinates)
  "Returns T if the `MATCHING', which is a LIST in the form ((X0 Y0) (X1 Y1)) where X0, Y0, X1, Y1 are all INTEGERs, contains all of the `COORDINATES', each of which is a LIST of INTEGERs in the form (X0 Y0). Uses the `CONTAINS-COORDINATE?' function to do this.."
  (every #'identity
         (loop :for coordinate :in coordinates
               :collect (contains-coordinate? matching coordinate))))

(defun await-matching (simulation match-address
                       &key coordinates offset timeout timestep)
  "Run the `SIMULATION' and listen on the `MATCH-ADDRESS' for a series of reap messages that contain the matching produced by the blossom algorithm. The simulation runs, checking the channel every `TIMESTEP' steps, until it received a perfect matching (determined using the LIST of `COORDINATES'), or until hitting the `TIMEOUT' (an INTEGER). Each message is unpacked (using the `OFFSET') into a LIST of LISTs, and pushed onto the MATCHING (also a LIST), which is returned at the end, along with the time it took to complete."
  (initialize-and-return ((final-time) (matching))
    (loop :with time := 0
          :do (when (or (perfect-matching? matching coordinates)
                        ;; break out if taking too long
                        (>= time timeout))
                (setf final-time time)
                (return t))
              (incf time timestep)
              (simulation-run simulation :canary (canary-until time))
              (receive-message (match-address reap-message)
                (message-reap
                 (push (unpack-match reap-message offset) matching))))))

(defun match< (a b)
  "Determines if match `A' should go before match `B', where both are LISTs in the form ((X0 Y0) (X1 Y1)) where X0, Y0, X1, and Y1 are all INTEGERs. First compares by closeness to origin, and then by X0."
  (let* ((distance-to-origin-a (+ (caar a) (cadar a)))
         (distance-to-origin-b (+ (caar b) (cadar b))))
    (cond
      ;; if you're closer to origin, you come first
      ((< distance-to-origin-a distance-to-origin-b)
       t)
      ;; break ties by comparing X0
      ((= distance-to-origin-a distance-to-origin-b)
       (< (caar a) (caar b)))
      (t
       nil))))

(defun sort-matching (matching)
  "Given a `MATCHING', which is a LIST in the form ((X0 Y0) (X1 Y1)) where X0, Y0, X1, Y1 are all INTEGERs, sort it using the `MATCH<' predicate."
  (sort (copy-list matching) #'match<))

(defun one-of (matching &rest correct-matchings)
  "Given a `MATCHING' and a &REST LIST of `CORRECT-MATCHINGS', return T if `MATCHING' is one of the `CORRECT-MATCHINGS'."
  (notevery #'null
            (loop :for correct-matching :in correct-matchings
                  :for sorted-correct-matching := (sort-matching correct-matching)
                  :for sorted-matching := (sort-matching matching)
                  :collect (equal sorted-matching sorted-correct-matching))))

(defmacro define-blossom-test (test-name coordinates
                               (&key (border +default-border+)
                                     (debug? nil)
                                     (dryad-clock-rate +default-dryad-clock-rate+)
                                     (iterations +default-iterations+)
                                     (timeout +default-timeout+)
                                     (timestep +default-timestep+)
                                     (dryad-class 'dryad)) &body body)
  "Used to define a blossom algorithm unit test named `TEST-NAME'. The LIST of `COORDINATES' represents the problem graph to be fed to the blossom algorithm.

There are also a collection of optional keyword arguments that allow individual tests to be customized. The `DEBUG?' and `DRYAD-CLOCK-RATE' parameters set the process debug flag and the process clock rate, respectively, of the DRYAD in the algorithm. The `ITERATIONS' parameter determines how many times the test will be run. The `BORDER' parameter offsets the coordinates. The `TIMEOUT' parameter determines how long the test can run before it is considered a failure. The `TIMESTEP' parameter designates how many clock cycles to run between each RECEIVE-MESSAGE call. The `DRYAD-CLASS' parameter allows this test suite to be used with different types of dryads.

Finally, the `BODY' contains optional declarations and a docstring, and then is followed by a series of LISTs, each of which represents a valid solution (i.e. minimum-weight perfect matching) for the problem graph. The `BODY' needs to contain at least one valid solution so that it can determine the correct minimum weight to check as part of the test. Test-writers can optionally provide additional solutions, but they currently do not play a part in the success or failure of a test.

NOTE: This macro automatically rescales the pairs in `COORDINATES' to reside at measurement qubit locations in a suitably large instance of the surface code."
  (multiple-value-bind (solutions decls docstring)
      (alexandria:parse-body body :documentation t)
    (assert (apply #'= (mapcar #'matching-weight solutions))
            ()
            "Solutions differ in weight.")
    (let ((solution-weight (matching-weight (first solutions))))
      `(deftest ,test-name ()
         ,@(when docstring (list docstring))
         ,@(when decls decls)
         (loop
           :for i :below ,iterations
           :with times
           :do (with-courier ()
                 (let* ((channel (register))
                        (dryad (spawn-process ',dryad-class
                                              :process-clock-rate ,dryad-clock-rate
                                              :match-address channel
                                              :debug? ,debug?)))
                   (with-simulation (simulation (*local-courier* dryad))
                     (anatevka::reset-logger)
                     (when (= 0 (mod i 50))
                       (trivial-garbage:gc :full t))
                     (sow-spacelike-graph dryad ',coordinates
                                          :offset ,border)
                     (multiple-value-bind (matching time)
                         (await-matching simulation channel
                                         :coordinates ',coordinates
                                         :offset ,border
                                         :timeout ,timeout
                                         :timestep ,timestep)
                       (push time times)
                       (unless matching
                         (error "No matching produced."))
                       (unless (one-of matching
                                       ,@(loop :for correct-matching :in solutions
                                               :collect `',correct-matching))
                         (unless (perfect-matching? matching ',coordinates)
                           (error "Found an imperfect matching: ~A" matching))
                         (when (< (matching-weight matching) ,solution-weight)
                           (error "Found better perfect matching: ~A has weight ~A<~A"
                                  matching
                                  (matching-weight matching)
                                  ,solution-weight))
                         (format t "~%Found perfect matching not in solutions: ~A~%"
                                 matching))
                       (is (= ,solution-weight (matching-weight matching)))))))
           :finally
              ;; print out run-time statistics
              (format t "~%runs: ~A~%mean: ~$~%std : ~$~%min : ~d~%max : ~d~%"
                      (length times)
                      (alexandria:mean times)
                      (alexandria:standard-deviation times)
                      (reduce #'min times)
                      (reduce #'max times)))))))

(define-blossom-test test-blossom-bulk-pair ((0 0) (0 1))
    (:border 1)
  "A pair of events in the bulk:

   o      o
   |  =>  |
   o      o
"
  (((0 0) (0 1))))

(define-blossom-test test-blossom-bulk-rectangle ((0 0) (0 1) (2 0) (2 1))
    (:border 1)
  "A rectangle of events in the bulk:

   o - - o      o    O
   |     |  =>  |    |
   o - - o      o    O
"
  (((0 0) (0 1)) ((2 0) (2 1))))

(define-blossom-test test-blossom-bulk-square ((0 0) (0 1) (1 0) (1 1))
    (:border 1)
  "A square of events in the bulk:

   o - o      o   O      o - o
   |   |  =>  |   |  ||
   o - o      o   O      O - O
"
  (((0 0) (0 1)) ((1 0) (1 1)))
  (((0 0) (1 0)) ((0 1) (1 1))))

(define-blossom-test test-blossom-bulk-square-tiling ((0 0) (0 1) (0 2)
                                                      (1 0) (1 1) (1 2))
    (:border 1)
  "A square tiling of events in the bulk:

   o - o      o - o      o   O      o - o
   |   |                 |   |
   o - o  =>  O   *  ||  o   O  ||  O - O
   |   |      |   |
   o - o      O   *      * - *      * - *
"
  (((0 0) (0 1)) ((1 0) (1 1)) ((0 2) (1 2)))
  (((0 0) (1 0)) ((0 1) (0 2)) ((1 1) (1 2)))
  (((0 0) (1 0)) ((0 1) (1 1)) ((0 2) (1 2))))

(define-blossom-test test-blossom-bulk-tbone ((0 0) (0 1) (0 2)
                                              (2 1) (3 1) (4 1))
    (:border 2)
  "A 'T-bone' configuration of events in the bulk:

   o                    o                    o                   0 - -
   |                    |                    |                         |
   o - - o - o - o  =>  O - - O   * - *  ||  o     O   * - * ||  o     0   * - *
   |                    |                          |             |
   o                    o                    O - -               o
"
  (((0 0) (0 2)) ((0 1) (2 1)) ((3 1) (4 1)))
  (((0 0) (2 1)) ((0 1) (0 2)) ((3 1) (4 1)))
  (((0 0) (0 1)) ((0 2) (2 1)) ((3 1) (4 1))))

(define-blossom-test test-blossom-bulk-bowtie ((0 0) (0 2) (1 1)
                                               (2 1) (3 0) (3 2))
    (:border 1)
  "A 'bowtie' configuration of events (>-<) in the bulk:

   o - +   + - o      o           *
       |   |          |           |
       o - o      =>  +   O - O   +
       |   |          |           |
   o - +   + - o      o           *
"
  (((0 0) (0 2)) ((1 1) (2 1)) ((3 0) (3 2))))

(define-blossom-test test-blossom-bulk-snake ((0 0) (2 0) (2 1) (4 1))
    (:border 1)
  "A snaking set of four events in the bulk:

         o - - o            O - - O
         |        =>
   o - - o            o - - o
"
  (((0 0) (2 0)) ((2 1) (4 1))))

(define-blossom-test test-blossom-bulk-hexagon ((0 1) (1 0) (2 1)
                                                (0 2) (1 3) (2 2))
    (:border 1)
  "A regular hexagon of events in the bulk:

       o              o              O              O
       |              |              |              |
   o - + - o      o - +   *      o   + - O      o   +   *
   |   |   |  =>          |  ||  |          ||  |   |   |
   o - + - o      O - +   *      o   + - *      o   +   *
       |              |              |              |
       o              O              *              O
"
  (((0 1) (1 0)) ((0 2) (1 3)) ((2 1) (2 2)))
  (((0 1) (0 2)) ((1 0) (2 1)) ((1 3) (2 2)))
  (((0 1) (0 2)) ((1 0) (1 3)) ((2 1) (2 2))))

(define-blossom-test test-blossom-bulk-pentagon ((0 0) (1 1) (2 0)
                                                 (0 2) (2 2) (1 3))
    (:border 2)
  "A regular right pentagon of events (plus a central one, forming a star) in the bulk:

      o
      |
  o - + - o
  |   |   |
  + - o - +  =>
  |   |   |
  o - + - o

      o              o - +      + - o              o              o              o
      |                  |      |                  |              |              |
  o - + - o      o - +   o      o   + - o      o - +   o      o   + - o      o   +   o
      |      ||      |      ||      |      ||          |  ||  |          ||  |   |   |
      o              o              o          + - o   +      +   o - +      +   o   +
                                               |       |      |       |      |       |
  o - + - o      o - + - o      o - + - o      o       o      o   +   o      o       o
"
  (((0 0) (2 0)) ((0 2) (2 2)) ((1 1) (1 3)))
  (((0 0) (2 0)) ((0 2) (1 1)) ((1 3) (2 2)))
  (((0 0) (2 0)) ((0 2) (1 3)) ((1 1) (2 2)))
  (((0 0) (1 1)) ((0 2) (1 3)) ((2 0) (2 2)))
  (((0 0) (0 2)) ((1 1) (2 0)) ((1 3) (2 2)))
  (((0 0) (0 2)) ((1 1) (1 3)) ((2 0) (2 2))))

(define-blossom-test test-blossom-bulk-triangle-tiling ((0 1) (1 0) (2 1)
                                                        (3 0) (4 1) (5 0))
    (:border 2)
  "A trianglular tiling of events in the bulk:

   o - + - o - + - o          o - +   0 - +   *
       |   |   |   |      =>      |       |   |
       o - + - o - + - o          o       0   + - *
"
  (((0 1) (1 0)) ((2 1) (3 0)) ((4 1) (5 0)))
  (((0 1) (2 1)) ((1 0) (3 0)) ((4 1) (5 0)))
  (((0 1) (1 0)) ((2 1) (4 1)) ((3 0) (5 0))))

(define-blossom-test test-blossom-bulk-diamond ((0 1) (1 0) (1 2) (2 1))
    (:border 1)
  "A diamond configuration of events in the bulk:

       o              0              0              o
       |              |              |              |
   o - + - o  =>  o - + - o  ||  o - + - 0  ||  o - + - 0
       |              |              |              |
       o              0              o              0
"
  (((0 1) (2 1)) ((1 0) (1 2)))
  (((0 1) (1 0)) ((1 2) (2 1)))
  (((0 1) (1 2)) ((1 0) (2 1))))

(define-blossom-test test-blossom-long-chain
    ((0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (8 0) (9 0))
    ()
  "A long linear chain of an even number of evenly spaced blossoms:

b - o - o - o - o - o - o - o - o - o - o - b

   =>

b   o - o   o - o   o - o   o - o   o - o   b
"
  (((0 0) (1 0)) ((2 0) (3 0)) ((4 0) (5 0)) ((6 0) (7 0)) ((8 0) (9 0))))
