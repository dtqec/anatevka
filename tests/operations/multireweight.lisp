;;;; tests/operations/multireweight.lisp
;;;;
;;;; Unit tests for the `MULTIREWEIGHT' supervisor action.

(in-package #:anatevka-tests)

(deftest test-supervisor-multireweight-interlock-aligned ()
  "Checks the transformation

  0       0       4               2       2       2
  +       +       -               +       +       -
  A       D <==== E <--           A       D <==== E <--
  ^                    |    -->   ^                    |
  !                    |          !                    |
   == B <---- C        F           == B <---- C        F
      -       +        +              -       +        +
      4       0        0              2       2        2

d(B, D) = 4 and d(C, E) = 4
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2)
              :match-edge (vv-edge A B)
              :parent (vv-edge A B))
           (B :id (id 2 0)
              :children (list (vv-edge B A))
              :internal-weight 4
              :match-edge (vv-edge B A)
              :parent (vv-edge B C)
              :positive? nil)
           (C :id (id 6 0)
              :children (list (vv-edge C B))
              :held-by-roots (list F)
              :paused? T)
           (D :id (id 4 2)
              :match-edge (vv-edge D E)
              :parent (vv-edge D E))
           (E :id (id 8 2)
              :children (list (vv-edge E D))
              :internal-weight 4
              :match-edge (vv-edge E D)
              :parent (vv-edge E F)
              :positive? nil)
           (F :id (id 10 0)
              :children (list (vv-edge F E))
              :held-by-roots (list C)))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':hold
                                      :weight 0
                                      :edges (list (vv-edge B D))
                                      :source-root (process-public-address C)
                                      :target-root (process-public-address F)
                                      :source-id (slot-value B 'anatevka::id)
                                      :root-bucket (list (process-public-address F)))))
          (simulate-add-tree simulation original-tree)

          ;; fill the dryad and add it to the simulation
          (dolist (node original-tree)
            (let* ((id (slot-value node 'anatevka::id))
                   (address (process-public-address node)))
              (setf (gethash address (anatevka::dryad-ids dryad))       id
                    (gethash address (anatevka::dryad-sprouted? dryad)) nil)))
          (simulation-add-event simulation (make-event :callback dryad))

          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0 2)
                  :match-edge (vv-edge A B)
                  :parent (vv-edge A B)
                  :internal-weight 2)
               (B :id (id 2 0)
                  :children (list (vv-edge B A))
                  :internal-weight 2
                  :match-edge (vv-edge B A)
                  :parent (vv-edge B C)
                  :positive? nil)
               (C :id (id 6 0)
                  :children (list (vv-edge C B))
                  :internal-weight 2)
               (D :id (id 4 2)
                  :internal-weight 2
                  :match-edge (vv-edge D E)
                  :parent (vv-edge D E))
               (E :id (id 8 2)
                  :children (list (vv-edge E D))
                  :internal-weight 2
                  :match-edge (vv-edge E D)
                  :parent (vv-edge E F)
                  :positive? nil)
               (F :id (id 10 0)
                  :internal-weight 2
                  :children (list (vv-edge F E))))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-multireweight-interlock-mirrored ()
  "Checks the transformation

  0        0       4               2        2       2
  +        +       -               +        +       -
  A        D <==== E <--           A        D <==== E <--
  |                     |    -->   |                     |
  |                     |          |                     |
   --> B ====> C        F           --> B ====> C        F
       -       +        +               -       +        +
       4       0        0               2       2        2

d(B, D) = 4 and d(C, E) = 4
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2)
              :children (list (vv-edge A B))
              :held-by-roots (list F)
              :paused? T)
           (B :id (id 2 0)
              :children (list (vv-edge B C))
              :internal-weight 4
              :match-edge (vv-edge B C)
              :parent (vv-edge B A)
              :positive? nil)
           (C :id (id 6 0)
              :match-edge (vv-edge C B)
              :parent (vv-edge C B))
           (D :id (id 4 2)
              :match-edge (vv-edge D E)
              :parent (vv-edge D E))
           (E :id (id 8 2)
              :children (list (vv-edge E D))
              :internal-weight 4
              :match-edge (vv-edge E D)
              :parent (vv-edge E F)
              :positive? nil)
           (F :id (id 10 0)
              :children (list (vv-edge F E))
              :held-by-roots (list A)))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':hold
                                      :weight 0
                                      :edges (list (vv-edge C E))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address F)
                                      :source-id (slot-value C 'anatevka::id)
                                      :root-bucket (list (process-public-address F)))))
          (simulate-add-tree simulation original-tree)

          ;; fill the dryad and add it to the simulation
          (dolist (node original-tree)
          (let* ((id (slot-value node 'anatevka::id))
                 (address (process-public-address node)))
            (setf (gethash address (anatevka::dryad-ids dryad))       id
                  (gethash address (anatevka::dryad-sprouted? dryad)) nil)))
          (simulation-add-event simulation (make-event :callback dryad))

          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0 2)
                  :children (list (vv-edge A B))
                  :internal-weight 2)
               (B :id (id 2 0)
                  :children (list (vv-edge B C))
                  :internal-weight 2
                  :match-edge (vv-edge B C)
                  :parent (vv-edge B A)
                  :positive? nil)
               (C :id (id 6 0)
                  :internal-weight 2
                  :match-edge (vv-edge C B)
                  :parent (vv-edge C B))
               (D :id (id 4 2)
                  :internal-weight 2
                  :match-edge (vv-edge D E)
                  :parent (vv-edge D E))
               (E :id (id 8 2)
                  :children (list (vv-edge E D))
                  :internal-weight 2
                  :match-edge (vv-edge E D)
                  :parent (vv-edge E F)
                  :positive? nil)
               (F :id (id 10 0)
                  :internal-weight 2
                  :children (list (vv-edge F E))))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-multireweight-outerlock-aligned ()
  "Checks the transformation

  0        0        4             2        2        2
  +        +        -             +        +        -
  A        D    --> E ==          A        D    --> E ==
  |        ^   |        !   -->   |        ^   |        !
  |        !   |        v         |        !   |        v
   --> B ==    C        F          --> B ==    C        F
       -       +        +              -       +        +
       4       0        0              2       2        2

d(C, D) = 4
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2)
              :children (list (vv-edge A B))
              :held-by-roots (list C)
              :paused? T)
           (B :id (id 2 0)
              :children (list (vv-edge B D))
              :internal-weight 4
              :match-edge (vv-edge B D)
              :parent (vv-edge B A)
              :positive? nil)
           (C :id (id 6 0)
              :children (list (vv-edge C E))
              :held-by-roots (list A))
           (D :id (id 4 2)
              :match-edge (vv-edge D B)
              :parent (vv-edge D B))
           (E :id (id 8 2)
              :children (list (vv-edge E F))
              :internal-weight 4
              :match-edge (vv-edge E F)
              :parent (vv-edge E C)
              :positive? nil)
           (F :id (id 10 0)
              :match-edge (vv-edge F E)
              :parent (vv-edge F E)))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':hold
                                      :weight 0
                                      :edges (list (vv-edge B C))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address C)
                                      :source-id (slot-value B 'anatevka::id)
                                      :root-bucket (list (process-public-address C)))))
          (simulate-add-tree simulation original-tree)

          ;; fill the dryad and add it to the simulation
          (dolist (node original-tree)
          (let* ((id (slot-value node 'anatevka::id))
                 (address (process-public-address node)))
            (setf (gethash address (anatevka::dryad-ids dryad))       id
                  (gethash address (anatevka::dryad-sprouted? dryad)) nil)))
          (simulation-add-event simulation (make-event :callback dryad))

          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0 2)
                  :children (list (vv-edge A B))
                  :internal-weight 2)
               (B :id (id 2 0)
                  :children (list (vv-edge B D))
                  :internal-weight 2
                  :match-edge (vv-edge B D)
                  :parent (vv-edge B A)
                  :positive? nil)
               (C :id (id 6 0)
                  :children (list (vv-edge C E))
                  :internal-weight 2)
               (D :id (id 4 2)
                  :internal-weight 2
                  :match-edge (vv-edge D B)
                  :parent (vv-edge D B))
               (E :id (id 8 2)
                  :children (list (vv-edge E F))
                  :internal-weight 2
                  :match-edge (vv-edge E F)
                  :parent (vv-edge E C)
                  :positive? nil)
               (F :id (id 10 0)
                  :internal-weight 2
                  :match-edge (vv-edge F E)
                  :parent (vv-edge F E)))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-multireweight-outerlock-mirrored ()
  "Checks the transformation

  0        0       4              2        2       2
  +        +       -              +        +       -
  A        D    == E <--          A        D    == E <--
  |        ^   !        |   -->   |        ^   !        |
  |        !   V        |         |        !   v        |
   --> B ==    C        F          --> B ==    C        F
       -       +        +              -       +        +
       4       0        0              2       2        2

d(C, D) = 4
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2)
              :children (list (vv-edge A B))
              :held-by-roots (list F)
              :paused? T)
           (B :id (id 2 0)
              :children (list (vv-edge B D))
              :internal-weight 4
              :match-edge (vv-edge B D)
              :parent (vv-edge B A)
              :positive? nil)
           (C :id (id 6 0)
              :match-edge (vv-edge C E)
              :parent (vv-edge C E))
           (D :id (id 4 2)
              :match-edge (vv-edge D B)
              :parent (vv-edge D B))
           (E :id (id 8 2)
              :children (list (vv-edge E C))
              :internal-weight 4
              :match-edge (vv-edge E C)
              :parent (vv-edge E F)
              :positive? nil)
           (F :id (id 10 0)
              :children (list (vv-edge F E))
              :held-by-roots (list A)))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':hold
                                      :weight 0
                                      :edges (list (vv-edge D E))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address F)
                                      :source-id (slot-value D 'anatevka::id)
                                      :root-bucket (list (process-public-address F)))))
          (simulate-add-tree simulation original-tree)

          ;; fill the dryad and add it to the simulation
          (dolist (node original-tree)
          (let* ((id (slot-value node 'anatevka::id))
                 (address (process-public-address node)))
            (setf (gethash address (anatevka::dryad-ids dryad))       id
                  (gethash address (anatevka::dryad-sprouted? dryad)) nil)))
          (simulation-add-event simulation (make-event :callback dryad))

          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0 2)
                  :children (list (vv-edge A B))
                  :internal-weight 2)
               (B :id (id 2 0)
                  :children (list (vv-edge B D))
                  :internal-weight 2
                  :match-edge (vv-edge B D)
                  :parent (vv-edge B A)
                  :positive? nil)
               (C :id (id 6 0)
                  :internal-weight 2
                  :match-edge (vv-edge C E)
                  :parent (vv-edge C E))
               (D :id (id 4 2)
                  :internal-weight 2
                  :match-edge (vv-edge D B)
                  :parent (vv-edge D B))
               (E :id (id 8 2)
                  :children (list (vv-edge E C))
                  :internal-weight 2
                  :match-edge (vv-edge E C)
                  :parent (vv-edge E F)
                  :positive? nil)
               (F :id (id 10 0)
                  :children (list (vv-edge F E))
                  :internal-weight 2))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-multireweight-full-external-rec ()
  "Checks the transformation

  0        0       4                   1        1       3
  +        +       -                   +        +       -
  A        D <==== E <--               A        D <==== E <--
  |                     |       -->    |                     |
  |                     |              |                     |
   --> B ====> C        F   G           --> B ====> C        F   G
       -       +        +   +               -       +        +   +
       4       0        0   0               3       1        1   0

d(B, D) = 4 and d(C, E) = 4 and d(F, G) = 1

The point of this test is to show that when the best recommendation in the
local area is not in the `root-set' of the `MULTIREWEIGHT' operation, we
should reweight all deadlocked trees by the full weight of that recommendation
rather than half of its weight (like we do for inter-tree recommendations).
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2)
              :children (list (vv-edge A B))
              :held-by-roots (list F)
              :paused? T)
           (B :id (id 2 0)
              :children (list (vv-edge B C))
              :internal-weight 4
              :match-edge (vv-edge B C)
              :parent (vv-edge B A)
              :positive? nil)
           (C :id (id 6 0)
              :match-edge (vv-edge C B)
              :parent (vv-edge C B))
           (D :id (id 4 2)
              :match-edge (vv-edge D E)
              :parent (vv-edge D E))
           (E :id (id 8 2)
              :children (list (vv-edge E D))
              :internal-weight 4
              :match-edge (vv-edge E D)
              :parent (vv-edge E F)
              :positive? nil)
           (F :id (id 10 0)
              :children (list (vv-edge F E))
              :held-by-roots (list A))
           (G :id (id 11 0)))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':hold
                                      :weight 0
                                      :edges (list (vv-edge C E))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address F)
                                      :source-id (slot-value C 'anatevka::id)
                                      :root-bucket (list (process-public-address F)))))
          (simulate-add-tree simulation original-tree)

          ;; fill the dryad and add it to the simulation
          (dolist (node original-tree)
          (let* ((id (slot-value node 'anatevka::id))
                 (address (process-public-address node))
                 (sprouted? (not (null (anatevka::blossom-node-match-edge node)))))
            (setf (gethash address (anatevka::dryad-ids dryad))       id
                  (gethash address (anatevka::dryad-sprouted? dryad)) sprouted?)))
          (simulation-add-event simulation (make-event :callback dryad))

          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0 2)
                  :children (list (vv-edge A B))
                  :internal-weight 1)
               (B :id (id 2 0)
                  :children (list (vv-edge B C))
                  :internal-weight 3
                  :match-edge (vv-edge B C)
                  :parent (vv-edge B A)
                  :positive? nil)
               (C :id (id 6 0)
                  :internal-weight 1
                  :match-edge (vv-edge C B)
                  :parent (vv-edge C B))
               (D :id (id 4 2)
                  :internal-weight 1
                  :match-edge (vv-edge D E)
                  :parent (vv-edge D E))
               (E :id (id 8 2)
                  :children (list (vv-edge E D))
                  :internal-weight 3
                  :match-edge (vv-edge E D)
                  :parent (vv-edge E F)
                  :positive? nil)
               (F :id (id 10 0)
                  :internal-weight 1
                  :children (list (vv-edge F E)))
               (G :id (id 11 0)))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-multireweight-contract-wins ()
  "Checks the transformation

  1        1         4             2        2        3
  +        +         -             +        +        -
  A        D     --> E ==          A        D    --> E ==
  |        ^    |        !         |        ^   |        !
  |        !    |        !   -->   |        !   |        !
  |        !    |        !         |        !   |        !
  |        !    |        v         |        !   |        v
   --> B ==     C        F          --> B ==    C        F
       -        +        +              -       +        +
       4        1        1              3       2        2

d(C, D) = 6 - 1 - 1 = 4, A = (2, 4)

The point of this test is to show that when the best recommendation is
`CONTRACT', we cannot actually ignore it. We can see that there are two weight-1
blossom-contraction operations available, but the only inter-tree operation is
an `AUGMENT' of distance 4. Inter-tree augmentation weights are halved, so the
effective weight is 2, but that is still higher than the `CONTRACT' weight.
Previously, the augmentation would have won because we were discarding blossom
contraction recommendations as if they were as inconsequential as internal
`HOLD's. However, they actually indicate real distance constraints, namely, the
distance between pairs of outer nodes in the same tree. Therefore, we cannot
reweight by 2, even though that is what the `AUGMENT' recommends, because our
`CONTRACT' tells us that would result in a negative-weight edge.
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 2 4)
              :internal-weight 1
              :children (list (vv-edge A B))
              :held-by-roots (list C)
              :paused? T)
           (B :id (id 4 0)
              :children (list (vv-edge B D))
              :internal-weight 5
              :match-edge (vv-edge B D)
              :parent (vv-edge B A)
              :positive? nil)
           (C :id (id 10 0)
              :internal-weight 1
              :children (list (vv-edge C E))
              :held-by-roots (list A))
           (D :id (id 6 4)
              :internal-weight 1
              :match-edge (vv-edge D B)
              :parent (vv-edge D B))
           (E :id (id 12 4)
              :children (list (vv-edge E F))
              :internal-weight 5
              :match-edge (vv-edge E F)
              :parent (vv-edge E C)
              :positive? nil)
           (F :id (id 14 0)
              :internal-weight 1
              :match-edge (vv-edge F E)
              :parent (vv-edge F E)))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':hold
                                      :weight 0
                                      :edges (list (vv-edge B C))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address C)
                                      :source-id (slot-value A 'anatevka::id)
                                      :root-bucket (list (process-public-address C)))))
          (simulate-add-tree simulation original-tree)

          ;; fill the dryad and add it to the simulation
          (dolist (node original-tree)
            (let* ((id (slot-value node 'anatevka::id))
                   (address (process-public-address node)))
              (setf (gethash address (anatevka::dryad-ids dryad))       id
                    (gethash address (anatevka::dryad-sprouted? dryad)) nil)))
          (simulation-add-event simulation (make-event :callback dryad))

          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 2 4)
                  :internal-weight 2
                  :children (list (vv-edge A B)))
               (B :id (id 4 0)
                  :children (list (vv-edge B D))
                  :internal-weight 4
                  :match-edge (vv-edge B D)
                  :parent (vv-edge B A)
                  :positive? nil)
               (C :id (id 10 0)
                  :internal-weight 2
                  :children (list (vv-edge C E)))
               (D :id (id 6 4)
                  :internal-weight 2
                  :match-edge (vv-edge D B)
                  :parent (vv-edge D B))
               (E :id (id 12 4)
                  :children (list (vv-edge E F))
                  :internal-weight 4
                  :match-edge (vv-edge E F)
                  :parent (vv-edge E C)
                  :positive? nil)
               (F :id (id 14 0)
                  :internal-weight 2
                  :match-edge (vv-edge F E)
                  :parent (vv-edge F E)))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-multireweight-simultaneous-aggregation ()
  "Checks the transformation

  0       0       4               2       2       2
  +       +       -               +       +       -
  A       D <==== E <--           A       D <==== E <--
  ^                    |    -->   ^                    |
  !                    |          !                    |
   == B <---- C        F           == B <---- C        F
      -       +        +              -       +        +
      4       0        0              2       2        2

d(B, D) = 4 and d(C, E) = 4

The point of this is to show that simultaneous root-set aggregation in multireweighting won't cause deadlock like it used to.
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2)
              :match-edge (vv-edge A B)
              :parent (vv-edge A B))
           (B :id (id 2 0)
              :children (list (vv-edge B A))
              :internal-weight 4
              :match-edge (vv-edge B A)
              :parent (vv-edge B C)
              :positive? nil)
           (C :id (id 6 0)
              :children (list (vv-edge C B))
              :held-by-roots (list F)
              :paused? T)
           (D :id (id 4 2)
              :match-edge (vv-edge D E)
              :parent (vv-edge D E))
           (E :id (id 8 2)
              :children (list (vv-edge E D))
              :internal-weight 4
              :match-edge (vv-edge E D)
              :parent (vv-edge E F)
              :positive? nil)
           (F :id (id 10 0)
              :children (list (vv-edge F E))
              :held-by-roots (list C)
              :paused? T))
        (let ((supervisor-left (supervisor simulation
                                           :recommendation ':hold
                                           :weight 0
                                           :edges (list (vv-edge D B))
                                           :source-root (process-public-address F)
                                           :target-root (process-public-address C)
                                           :source-id (slot-value D 'anatevka::id)
                                           :root-bucket (list
                                                         (process-public-address C))))
              (supervisor-right (supervisor simulation
                                           :recommendation ':hold
                                           :weight 0
                                           :edges (list (vv-edge B D))
                                           :source-root (process-public-address C)
                                           :target-root (process-public-address F)
                                           :source-id (slot-value B 'anatevka::id)
                                           :root-bucket (list (process-public-address F)))))
          (simulate-add-tree simulation original-tree)

          ;; fill the dryad and add it to the simulation
          (dolist (node original-tree)
          (let* ((id (slot-value node 'anatevka::id))
                 (address (process-public-address node))
                 (sprouted? (not (null (anatevka::blossom-node-match-edge node)))))
            (setf (gethash address (anatevka::dryad-ids dryad))       id
                  (gethash address (anatevka::dryad-sprouted? dryad)) sprouted?)))
          (simulation-add-event simulation (make-event :callback dryad))

          (simulate-until-dead simulation supervisor-left)
          (simulate-until-dead simulation supervisor-right)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0 2)
                  :match-edge (vv-edge A B)
                  :parent (vv-edge A B)
                  :internal-weight 2)
               (B :id (id 2 0)
                  :children (list (vv-edge B A))
                  :internal-weight 2
                  :match-edge (vv-edge B A)
                  :parent (vv-edge B C)
                  :positive? nil)
               (C :id (id 6 0)
                  :children (list (vv-edge C B))
                  :internal-weight 2)
               (D :id (id 4 2)
                  :internal-weight 2
                  :match-edge (vv-edge D E)
                  :parent (vv-edge D E))
               (E :id (id 8 2)
                  :children (list (vv-edge E D))
                  :internal-weight 2
                  :match-edge (vv-edge E D)
                  :parent (vv-edge E F)
                  :positive? nil)
               (F :id (id 10 0)
                  :internal-weight 2
                  :children (list (vv-edge F E))))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-multireweight-simultaneous-staircase ()
  "Checks the transformation

 0    2    0                                  2    0    2
 +    -    +                                  +    -    +
 A -> B => C                                  A -> B => C

      D -> E => F                                  D -> E => F
      +    -    +                                  +    -    +
      0    2    0                                  2    0    2
                                        -->
           G <= H <- I                                  G <= H <- I
           +    -    +                                  +    -    +
           0    2    0                                  2    0    2

                J <= K <- L                                  J <= K <- L
                +    -    +                                  +    -    +
                0    2    0                                  2    0    2

d(B, D), d(E, G), d(F, H), d(I, K)  = 2

The point of this is to show that we can successfully gather a root set of size greater than two, and even if two supervisors attempt to gather it simultaneously, one will always prevail."
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 6)
              :children (list (vv-edge A B))
              :held-by-roots (list D)
              :paused? T)
           (B :id (id 2 6)
              :children (list (vv-edge B C))
              :internal-weight 2
              :match-edge (vv-edge B C)
              :parent (vv-edge B A)
              :positive? nil)
           (C :id (id 4 6)
              :match-edge (vv-edge C B)
              :parent (vv-edge C B))
           (D :id (id 2 4)
              :children (list (vv-edge D E))
              :held-by-roots (list A I))
           (E :id (id 4 4)
              :children (list (vv-edge E F))
              :internal-weight 2
              :match-edge (vv-edge E F)
              :parent (vv-edge E D)
              :positive? nil)
           (F :id (id 6 4)
              :match-edge (vv-edge F E)
              :parent (vv-edge F E))
           (G :id (id 4 2)
              :match-edge (vv-edge G H)
              :parent (vv-edge G H))
           (H :id (id 6 2)
              :children (list (vv-edge H G))
              :internal-weight 2
              :match-edge (vv-edge H G)
              :parent (vv-edge H I)
              :positive? nil)
           (I :id (id 8 2)
              :children (list (vv-edge I H))
              :held-by-roots (list D L))
           (J :id (id 6 0)
              :match-edge (vv-edge J K)
              :parent (vv-edge J K))
           (K :id (id 8 0)
              :children (list (vv-edge K J))
              :internal-weight 2
              :match-edge (vv-edge K J)
              :parent (vv-edge K L)
              :positive? nil)
           (L :id (id 10 0)
              :children (list (vv-edge L K))
              :held-by-roots (list I)
              :paused? T))

        (let ((supervisor-left (supervisor simulation
                                           :recommendation ':hold
                                           :weight 0
                                           :edges (list (vv-edge B D))
                                           :source-root (process-public-address A)
                                           :target-root (process-public-address D)
                                           :source-id (slot-value B 'anatevka::id)
                                           :root-bucket (list
                                                         (process-public-address D))))
              (supervisor-right (supervisor simulation
                                           :recommendation ':hold
                                           :weight 0
                                           :edges (list (vv-edge K I))
                                           :source-root (process-public-address L)
                                           :target-root (process-public-address I)
                                           :source-id (slot-value K 'anatevka::id)
                                           :root-bucket (list (process-public-address I)))))
          (simulate-add-tree simulation original-tree)

          ;; fill the dryad and add it to the simulation
          (dolist (node original-tree)
          (let* ((id (slot-value node 'anatevka::id))
                 (address (process-public-address node))
                 (sprouted? (not (null (anatevka::blossom-node-match-edge node)))))
            (setf (gethash address (anatevka::dryad-ids dryad))       id
                  (gethash address (anatevka::dryad-sprouted? dryad)) sprouted?)))
          (simulation-add-event simulation (make-event :callback dryad))

          (simulate-until-dead simulation supervisor-left)
          (simulate-until-dead simulation supervisor-right)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0 6)
                  :internal-weight 2
                  :children (list (vv-edge A B)))
               (B :id (id 2 6)
                  :children (list (vv-edge B C))
                  :match-edge (vv-edge B C)
                  :parent (vv-edge B A)
                  :positive? nil)
               (C :id (id 4 6)
                  :internal-weight 2
                  :match-edge (vv-edge C B)
                  :parent (vv-edge C B))
               (D :id (id 2 4)
                  :internal-weight 2
                  :children (list (vv-edge D E)))
               (E :id (id 4 4)
                  :children (list (vv-edge E F))
                  :match-edge (vv-edge E F)
                  :parent (vv-edge E D)
                  :positive? nil)
               (F :id (id 6 4)
                  :internal-weight 2
                  :match-edge (vv-edge F E)
                  :parent (vv-edge F E))
               (G :id (id 4 2)
                  :internal-weight 2
                  :match-edge (vv-edge G H)
                  :parent (vv-edge G H))
               (H :id (id 6 2)
                  :children (list (vv-edge H G))
                  :match-edge (vv-edge H G)
                  :parent (vv-edge H I)
                  :positive? nil)
               (I :id (id 8 2)
                  :internal-weight 2
                  :children (list (vv-edge I H)))
               (J :id (id 6 0)
                  :internal-weight 2
                  :match-edge (vv-edge J K)
                  :parent (vv-edge J K))
               (K :id (id 8 0)
                  :children (list (vv-edge K J))
                  :match-edge (vv-edge K J)
                  :parent (vv-edge K L)
                  :positive? nil)
               (L :id (id 10 0)
                  :internal-weight 2
                  :children (list (vv-edge L K))))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-multireweight-lower-priority ()
  "Check that when F receives a multireweight proposal in

  0       0       4               2       2       2
  +       +       -               +       +       -
  A       D <==== E <--           A       D <==== E <--
  ^                    |    -->   ^                    |
  !                    |          !                    |
   == B <---- C        F           == B <---- C        F
      -       +        +              -       +        +
      4       0        0              2       2        2

d(B, D) = 4 and d(C, E) = 4,

it declines to take action because C has priority.
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2)
              :match-edge (vv-edge A B)
              :parent (vv-edge A B))
           (B :id (id 2 0)
              :children (list (vv-edge B A))
              :internal-weight 4
              :match-edge (vv-edge B A)
              :parent (vv-edge B C)
              :positive? nil)
           (C :id (id 6 0)
              :children (list (vv-edge C B))
              :held-by-roots (list F))
           (D :id (id 4 2)
              :match-edge (vv-edge D E)
              :parent (vv-edge D E))
           (E :id (id 8 2)
              :children (list (vv-edge E D))
              :internal-weight 4
              :match-edge (vv-edge E D)
              :parent (vv-edge E F)
              :positive? nil)
           (F :id (id 10 0)
              :children (list (vv-edge F E))
              :held-by-roots (list C)
              :paused? T))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':hold
                                      :weight 0
                                      :edges (list (vv-edge D B))
                                      :source-root (process-public-address F)
                                      :target-root (process-public-address C)
                                      :source-id (slot-value D 'anatevka::id)
                                      :root-bucket (list (process-public-address C)))))
          (simulate-add-tree simulation original-tree)

          ;; fill the dryad and add it to the simulation
          (dolist (node original-tree)
            (let* ((id (slot-value node 'anatevka::id))
                   (address (process-public-address node)))
              (setf (gethash address (anatevka::dryad-ids dryad))       id
                    (gethash address (anatevka::dryad-sprouted? dryad)) nil)))
          (simulation-add-event simulation (make-event :callback dryad))

          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((AA :id (id 0 2)
                   :match-edge (vv-edge AA BB)
                   :parent (vv-edge AA BB))
               (BB :id (id 2 0)
                   :children (list (vv-edge BB AA))
                   :internal-weight 4
                   :match-edge (vv-edge BB AA)
                   :parent (vv-edge BB CC)
                   :positive? nil)
               (CC :id (id 6 0)
                   :children (list (vv-edge CC BB))
                   :held-by-roots (list FF))
               (DD :id (id 4 2)
                   :match-edge (vv-edge DD EE)
                   :parent (vv-edge DD EE))
               (EE :id (id 8 2)
                   :children (list (vv-edge EE DD))
                   :internal-weight 4
                   :match-edge (vv-edge EE DD)
                   :parent (vv-edge EE FF)
                   :positive? nil)
               (FF :id (id 10 0)
                   :children (list (vv-edge FF EE))
                   :held-by-roots (list CC)))
            (is (tree-equalp original-tree target-tree))))))))

;;;
;;; multi-cluster tests (internal pong is internal)
;;;

;; NB: this test is in main as-is, and should fail after the changes
;;     (post-changes one cluster should reweight by 2)
;;     NVM these stay the same because the internal pong is internal
(deftest test-supervisor-multireweight-simultaneous-rewind-halfway ()
  "Checks the transformation

 0    2    0              0    2    0         1    1    1              1    1    1
 +    -    +              +    -    +         +    -    +              +    -    +
 A -> B => C              J <= K <- L         A -> B => C              J <= K <- L
                                        -->
      D -> E => F    G <= H <- I                   D -> E => F    G <= H <- I
      +    -    +    +    -    +                   +    -    +    +    -    +
      0    2    0    0    2    0                   1    1    1    1    1    1

d(B, D), d(F, G), d(H, J)  = 2

The point of this is to show that simultaneous reweighting and rewinding during multireweighting won't cause a negative-weight edge (all roots in the root-set are rewound) and for inter-root-set distances > 1 the algorithm will progress.
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2)
              :children (list (vv-edge A B))
              :held-by-roots (list D)
              :paused? T)
           (B :id (id 2 2)
              :children (list (vv-edge B C))
              :internal-weight 2
              :match-edge (vv-edge B C)
              :parent (vv-edge B A)
              :positive? nil)
           (C :id (id 4 2)
              :match-edge (vv-edge C B)
              :parent (vv-edge C B))
           (D :id (id 2 0)
              :children (list (vv-edge D E))
              :held-by-roots (list A))
           (E :id (id 4 0)
              :children (list (vv-edge E F))
              :internal-weight 2
              :match-edge (vv-edge E F)
              :parent (vv-edge E D)
              :positive? nil)
           (F :id (id 6 0)
              :match-edge (vv-edge F E)
              :parent (vv-edge F E))
           (G :id (id 8 0)
              :match-edge (vv-edge G H)
              :parent (vv-edge G H))
           (H :id (id 10 0)
              :children (list (vv-edge H G))
              :internal-weight 2
              :match-edge (vv-edge H G)
              :parent (vv-edge H I)
              :positive? nil)
           (I :id (id 12 0)
              :children (list (vv-edge I H))
              :held-by-roots (list L)
              :paused? T)
           (J :id (id 10 2)
              :match-edge (vv-edge J K)
              :parent (vv-edge J K))
           (K :id (id 12 2)
              :children (list (vv-edge K J))
              :internal-weight 2
              :match-edge (vv-edge K J)
              :parent (vv-edge K L)
              :positive? nil)
           (L :id (id 14 2)
              :children (list (vv-edge L K))
              :held-by-roots (list I)))

        (let ((supervisor-left (supervisor simulation
                                           :recommendation ':hold
                                           :weight 0
                                           :edges (list (vv-edge B D))
                                           :source-root (process-public-address A)
                                           :target-root (process-public-address D)
                                           :source-id (slot-value B 'anatevka::id)
                                           :root-bucket (list
                                                         (process-public-address D))))
              (supervisor-right (supervisor simulation
                                           :recommendation ':hold
                                           :weight 0
                                           :edges (list (vv-edge I K))
                                           :source-root (process-public-address I)
                                           :target-root (process-public-address L)
                                           :source-id (slot-value I 'anatevka::id)
                                           :root-bucket (list (process-public-address L)))))
          (simulate-add-tree simulation original-tree)

          ;; fill the dryad and add it to the simulation
          (dolist (node original-tree)
          (let* ((id (slot-value node 'anatevka::id))
                 (address (process-public-address node))
                 (sprouted? (not (null (anatevka::blossom-node-match-edge node)))))
            (setf (gethash address (anatevka::dryad-ids dryad))       id
                  (gethash address (anatevka::dryad-sprouted? dryad)) sprouted?)))
          (simulation-add-event simulation (make-event :callback dryad))

          (simulate-until-dead simulation supervisor-left)
          (simulate-until-dead simulation supervisor-right)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0 2)
                  :internal-weight 1
                  :children (list (vv-edge A B)))
               (B :id (id 2 2)
                  :children (list (vv-edge B C))
                  :internal-weight 1
                  :match-edge (vv-edge B C)
                  :parent (vv-edge B A)
                  :positive? nil)
               (C :id (id 4 2)
                  :internal-weight 1
                  :match-edge (vv-edge C B)
                  :parent (vv-edge C B))
               (D :id (id 2 0)
                  :internal-weight 1
                  :children (list (vv-edge D E)))
               (E :id (id 4 0)
                  :children (list (vv-edge E F))
                  :internal-weight 1
                  :match-edge (vv-edge E F)
                  :parent (vv-edge E D)
                  :positive? nil)
               (F :id (id 6 0)
                  :internal-weight 1
                  :match-edge (vv-edge F E)
                  :parent (vv-edge F E))
               (G :id (id 8 0)
                  :internal-weight 1
                  :match-edge (vv-edge G H)
                  :parent (vv-edge G H))
               (H :id (id 10 0)
                  :children (list (vv-edge H G))
                  :internal-weight 1
                  :match-edge (vv-edge H G)
                  :parent (vv-edge H I)
                  :positive? nil)
               (I :id (id 12 0)
                  :internal-weight 1
                  :children (list (vv-edge I H)))
               (J :id (id 10 2)
                  :internal-weight 1
                  :match-edge (vv-edge J K)
                  :parent (vv-edge J K))
               (K :id (id 12 2)
                  :children (list (vv-edge K J))
                  :internal-weight 1
                  :match-edge (vv-edge K J)
                  :parent (vv-edge K L)
                  :positive? nil)
               (L :id (id 14 2)
                  :internal-weight 1
                  :children (list (vv-edge L K))))
            (is (tree-equalp original-tree target-tree))))))))

;; NB: this test is in main as-is, and should fail after the changes
;;     (post-changes one cluster should reweight by 1)
(deftest test-supervisor-multireweight-simultaneous-rewind-non-integer ()
  "Checks the transformation

 0    1    0              0    1    0        0.5  0.5  0.5            0.5  0.5  0.5
 +    -    +              +    -    +         +    -    +              +    -    +
 A -> B => C              J <= K <- L         A -> B => C              J <= K <- L
                                        -->
      D -> E => F    G <= H <- I                   D -> E => F    G <= H <- I
      +    -    +    +    -    +                   +    -    +    +    -    +
      0    1    0    0    1    0                  0.5  0.5  0.5  0.5  0.5  0.5

d(B, D), d(F, G), d(H, J)  = 1

The point of this is to show that simultaneous reweighting and rewinding during multireweighting will still ensure progress even if the inter-root-set distance is 1 (or smaller) by using non-integer rewinds.
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 1)
              :children (list (vv-edge A B))
              :held-by-roots (list D)
              :paused? T)
           (B :id (id 1 1)
              :children (list (vv-edge B C))
              :internal-weight 1
              :match-edge (vv-edge B C)
              :parent (vv-edge B A)
              :positive? nil)
           (C :id (id 2 1)
              :match-edge (vv-edge C B)
              :parent (vv-edge C B))
           (D :id (id 1 0)
              :children (list (vv-edge D E))
              :held-by-roots (list A))
           (E :id (id 2 0)
              :children (list (vv-edge E F))
              :internal-weight 1
              :match-edge (vv-edge E F)
              :parent (vv-edge E D)
              :positive? nil)
           (F :id (id 3 0)
              :match-edge (vv-edge F E)
              :parent (vv-edge F E))
           (G :id (id 4 0)
              :match-edge (vv-edge G H)
              :parent (vv-edge G H))
           (H :id (id 5 0)
              :children (list (vv-edge H G))
              :internal-weight 1
              :match-edge (vv-edge H G)
              :parent (vv-edge H I)
              :positive? nil)
           (I :id (id 6 0)
              :children (list (vv-edge I H))
              :held-by-roots (list L)
              :paused? T)
           (J :id (id 5 1)
              :match-edge (vv-edge J K)
              :parent (vv-edge J K))
           (K :id (id 6 1)
              :children (list (vv-edge K J))
              :internal-weight 1
              :match-edge (vv-edge K J)
              :parent (vv-edge K L)
              :positive? nil)
           (L :id (id 7 1)
              :children (list (vv-edge L K))
              :held-by-roots (list I)))

        (let ((supervisor-left (supervisor simulation
                                           :recommendation ':hold
                                           :weight 0
                                           :edges (list (vv-edge B D))
                                           :source-root (process-public-address A)
                                           :target-root (process-public-address D)
                                           :source-id (slot-value B 'anatevka::id)
                                           :root-bucket (list
                                                         (process-public-address D))))
              (supervisor-right (supervisor simulation
                                           :recommendation ':hold
                                           :weight 0
                                           :edges (list (vv-edge I K))
                                           :source-root (process-public-address I)
                                           :target-root (process-public-address L)
                                           :source-id (slot-value I 'anatevka::id)
                                           :root-bucket (list (process-public-address L)))))
          (simulate-add-tree simulation original-tree)

          ;; fill the dryad and add it to the simulation
          (dolist (node original-tree)
          (let* ((id (slot-value node 'anatevka::id))
                 (address (process-public-address node))
                 (sprouted? (not (null (anatevka::blossom-node-match-edge node)))))
            (setf (gethash address (anatevka::dryad-ids dryad))       id
                  (gethash address (anatevka::dryad-sprouted? dryad)) sprouted?)))
          (simulation-add-event simulation (make-event :callback dryad))

          (simulate-until-dead simulation supervisor-left)
          (simulate-until-dead simulation supervisor-right)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0 1)
                  :internal-weight 0.5
                  :children (list (vv-edge A B)))
               (B :id (id 1 1)
                  :internal-weight 0.5
                  :children (list (vv-edge B C))
                  :match-edge (vv-edge B C)
                  :parent (vv-edge B A)
                  :positive? nil)
               (C :id (id 2 1)
                  :internal-weight 0.5
                  :match-edge (vv-edge C B)
                  :parent (vv-edge C B))
               (D :id (id 1 0)
                  :internal-weight 0.5
                  :children (list (vv-edge D E)))
               (E :id (id 2 0)
                  :internal-weight 0.5
                  :children (list (vv-edge E F))
                  :match-edge (vv-edge E F)
                  :parent (vv-edge E D)
                  :positive? nil)
               (F :id (id 3 0)
                  :internal-weight 0.5
                  :match-edge (vv-edge F E)
                  :parent (vv-edge F E))
               (G :id (id 4 0)
                  :internal-weight 0.5
                  :match-edge (vv-edge G H)
                  :parent (vv-edge G H))
               (H :id (id 5 0)
                  :internal-weight 0.5
                  :children (list (vv-edge H G))
                  :match-edge (vv-edge H G)
                  :parent (vv-edge H I)
                  :positive? nil)
               (I :id (id 6 0)
                  :internal-weight 0.5
                  :children (list (vv-edge I H)))
               (J :id (id 5 1)
                  :internal-weight 0.5
                  :match-edge (vv-edge J K)
                  :parent (vv-edge J K))
               (K :id (id 6 1)
                  :internal-weight 0.5
                  :children (list (vv-edge K J))
                  :match-edge (vv-edge K J)
                  :parent (vv-edge K L)
                  :positive? nil)
               (L :id (id 7 1)
                  :internal-weight 0.5
                  :children (list (vv-edge L K)))
               (BOUNDARY :id (id -1 0)
                         :match-edge (vv-edge B A)
                         :parent (vv-edge B A)))
            (is (tree-equalp original-tree target-tree))))))))

;;;
;;; multi-cluster tests (internal-pong is external)

;; NB: this is a new test, and should fail after the changes
;;     (post-changes one cluster should reweight by 1)
(deftest test-supervisor-multireweight-2-clusters ()
  "Checks the transformation

 0  2  0              0  2  0     0  0  0              1  1  1
 +  -  +              +  -  +     +  -  +              +  -  +
 A->B=>C              L<=M<-N     A->B=>C              L<=M<-N
                              -->
    D->E=>F  G<=H<-I<=J<-K           D->E=>F  G<=H<-I<=J<-K
    +  -  +  +  -  +  -  +           +  -  +  +  -  +  -  +
    0  2  0  0  2  0  2  0           0  0  0  1  1  1  1  1

d(B, D), d(J, L) = 2 and d(F, G) = 1
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2)
              :children (list (vv-edge A B))
              :held-by-roots (list D)
              :paused? T)
           (B :id (id 2 2)
              :children (list (vv-edge B C))
              :internal-weight 2
              :match-edge (vv-edge B C)
              :parent (vv-edge B A)
              :positive? nil)
           (C :id (id 4 2)
              :match-edge (vv-edge C B)
              :parent (vv-edge C B))
           (D :id (id 2 0)
              :children (list (vv-edge D E))
              :held-by-roots (list A))
           (E :id (id 4 0)
              :children (list (vv-edge E F))
              :internal-weight 2
              :match-edge (vv-edge E F)
              :parent (vv-edge E D)
              :positive? nil)
           (F :id (id 6 0)
              :match-edge (vv-edge F E)
              :parent (vv-edge F E))
           (G :id (id 7 0)
              :match-edge (vv-edge G H)
              :parent (vv-edge G H))
           (H :id (id 9 0)
              :children (list (vv-edge H G))
              :internal-weight 2
              :match-edge (vv-edge H G)
              :parent (vv-edge H I)
              :positive? nil)
           (I :id (id 11 0)
              :children (list (vv-edge I H))
              :match-edge (vv-edge I J)
              :parent (vv-edge I J))
           (J :id (id 13 0)
              :internal-weight 2
              :children (list (vv-edge J I))
              :match-edge (vv-edge J I)
              :parent (vv-edge J K)
              :positive? nil)
           (K :id (id 15 0)
              :children (list (vv-edge K J))
              :held-by-roots (list N)
              :paused? T)
           (L :id (id 13 2)
              :match-edge (vv-edge L M)
              :parent (vv-edge L M))
           (M :id (id 15 2)
              :children (list (vv-edge M L))
              :internal-weight 2
              :match-edge (vv-edge M L)
              :parent (vv-edge M N)
              :positive? nil)
           (N :id (id 17 2)
              :children (list (vv-edge N M))
              :held-by-roots (list K)))
        (let ((supervisor-left (supervisor simulation :time 1/2
                                           :recommendation ':hold
                                           :weight 0
                                           :edges (list (vv-edge B D))
                                           :source-root (process-public-address A)
                                           :target-root (process-public-address D)
                                           :source-id (slot-value B 'anatevka::id)
                                           :root-bucket (list
                                                         (process-public-address D))))
              (supervisor-right (supervisor simulation :time 0
                                           :recommendation ':hold
                                           :weight 0
                                           :edges (list (vv-edge J L))
                                           :source-root (process-public-address K)
                                           :target-root (process-public-address N)
                                           :source-id (slot-value K 'anatevka::id)
                                           :root-bucket (list (process-public-address N)))))
          (simulate-add-tree simulation original-tree)

          ;; fill the dryad and add it to the simulation
          (dolist (node original-tree)
          (let* ((id (slot-value node 'anatevka::id))
                 (address (process-public-address node))
                 (sprouted? (not (null (anatevka::blossom-node-match-edge node)))))
            (setf (gethash address (anatevka::dryad-ids dryad))       id
                  (gethash address (anatevka::dryad-sprouted? dryad)) sprouted?)))
          (simulation-add-event simulation (make-event :callback dryad))

          (simulate-until-dead simulation supervisor-left)
          (simulate-until-dead simulation supervisor-right)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0 2)
                  :children (list (vv-edge A B)))
               (B :id (id 2 2)
                  :children (list (vv-edge B C))
                  :internal-weight 2
                  :match-edge (vv-edge B C)
                  :parent (vv-edge B A)
                  :positive? nil)
               (C :id (id 4 2)
                  :match-edge (vv-edge C B)
                  :parent (vv-edge C B))
               (D :id (id 2 0)
                  :children (list (vv-edge D E)))
               (E :id (id 4 0)
                  :children (list (vv-edge E F))
                  :internal-weight 2
                  :match-edge (vv-edge E F)
                  :parent (vv-edge E D)
                  :positive? nil)
               (F :id (id 6 0)
                  :match-edge (vv-edge F E)
                  :parent (vv-edge F E))
               (G :id (id 7 0)
                  :internal-weight 1
                  :match-edge (vv-edge G H)
                  :parent (vv-edge G H))
               (H :id (id 9 0)
                  :children (list (vv-edge H G))
                  :internal-weight 1
                  :match-edge (vv-edge H G)
                  :parent (vv-edge H I)
                  :positive? nil)
               (I :id (id 11 0)
                  :internal-weight 1
                  :children (list (vv-edge I H))
                  :match-edge (vv-edge I J)
                  :parent (vv-edge I J))
               (J :id (id 13 0)
                  :internal-weight 1
                  :children (list (vv-edge J I))
                  :match-edge (vv-edge J I)
                  :parent (vv-edge J K)
                  :positive? nil)
               (K :id (id 15 0)
                  :internal-weight 1
                  :children (list (vv-edge K J)))
               (L :id (id 13 2)
                  :internal-weight 1
                  :match-edge (vv-edge L M)
                  :parent (vv-edge L M))
               (M :id (id 15 2)
                  :children (list (vv-edge M L))
                  :internal-weight 1
                  :match-edge (vv-edge M L)
                  :parent (vv-edge M N)
                  :positive? nil)
               (N :id (id 17 2)
                  :internal-weight 1
                  :children (list (vv-edge N M))))
            (is (tree-equalp original-tree target-tree))))))))

;; NB: this is a new test, and should fail after the changes
;;     (post-changes one left cluster and the right cluster should reweight by 1)
(deftest test-supervisor-multireweight-3-clusters ()
  "Checks that this configuration will yield all 0->1/2 and all 2->3/2 for the left two clusters (A,D) and (I,L), and all 0->3/2 and all 1->1/2 for the rightmost cluster (M,P)

 0  2  0        0  2  0   0  2  0
 +  -  +        +  -  +   +  -  +
 A->B=>C        J<=K<-L   M->N=>O

    D->E=>F  G<=H<-I         P->Q=>R
    +  -  +  +  -  +         +  -  +
    0  2  0  0  2  0         0  2  0

d(B, D), d(H, J), d(N, P) = 2 and d(F, G), d(L, M) = 1
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2)
              :children (list (vv-edge A B))
              :held-by-roots (list D)
              :paused? T)
           (B :id (id 2 2)
              :children (list (vv-edge B C))
              :internal-weight 2
              :match-edge (vv-edge B C)
              :parent (vv-edge B A)
              :positive? nil)
           (C :id (id 4 2)
              :match-edge (vv-edge C B)
              :parent (vv-edge C B))
           (D :id (id 2 0)
              :children (list (vv-edge D E))
              :held-by-roots (list A))
           (E :id (id 4 0)
              :children (list (vv-edge E F))
              :internal-weight 2
              :match-edge (vv-edge E F)
              :parent (vv-edge E D)
              :positive? nil)
           (F :id (id 6 0)
              :match-edge (vv-edge F E)
              :parent (vv-edge F E))
           (G :id (id 7 0)
              :match-edge (vv-edge G H)
              :parent (vv-edge G H))
           (H :id (id 9 0)
              :children (list (vv-edge H G))
              :internal-weight 2
              :match-edge (vv-edge H G)
              :parent (vv-edge H I)
              :positive? nil)
           (I :id (id 11 0)
              :children (list (vv-edge I H))
              :held-by-roots (list L)
              :paused? T)
           (J :id (id 9 2)
              :match-edge (vv-edge J K)
              :parent (vv-edge J K))
           (K :id (id 11 2)
              :children (list (vv-edge K J))
              :internal-weight 2
              :match-edge (vv-edge K J)
              :parent (vv-edge K L)
              :positive? nil)
           (L :id (id 13 2)
              :children (list (vv-edge L K))
              :held-by-roots (list I))
           (M :id (id 14 2)
              :children (list (vv-edge M N))
              :held-by-roots (list P)
              :paused? T)
           (N :id (id 16 2)
              :children (list (vv-edge N O))
              :internal-weight 2
              :match-edge (vv-edge N O)
              :parent (vv-edge N M)
              :positive? nil)
           (O :id (id 18 2)
              :match-edge (vv-edge O N)
              :parent (vv-edge O N))
           (P :id (id 16 0)
              :children (list (vv-edge P Q))
              :held-by-roots (list M))
           (Q :id (id 18 0)
              :children (list (vv-edge Q R))
              :internal-weight 2
              :match-edge (vv-edge Q R)
              :parent (vv-edge Q P)
              :positive? nil)
           (R :id (id 20 0)
              :match-edge (vv-edge R Q)
              :parent (vv-edge R Q)))
        (let ((supervisor-A (supervisor simulation
                                        :recommendation ':hold
                                        :weight 0
                                        :edges (list (vv-edge B D))
                                        :source-root (process-public-address A)
                                        :target-root (process-public-address D)
                                        :source-id (slot-value B 'anatevka::id)
                                        :root-bucket (list
                                                      (process-public-address D))))
              (supervisor-I (supervisor simulation :time 1/2
                                        :recommendation ':hold
                                        :weight 0
                                        :edges (list (vv-edge I K))
                                        :source-root (process-public-address I)
                                        :target-root (process-public-address L)
                                        :source-id (slot-value I 'anatevka::id)
                                        :root-bucket (list (process-public-address L))))
              (supervisor-M (supervisor simulation :time 1
                                        :recommendation ':hold
                                        :weight 0
                                        :edges (list (vv-edge N P))
                                        :source-root (process-public-address M)
                                        :target-root (process-public-address P)
                                        :source-id (slot-value N 'anatevka::id)
                                        :root-bucket (list
                                                      (process-public-address P)))))
          (simulate-add-tree simulation original-tree)

          ;; fill the dryad and add it to the simulation
          (dolist (node original-tree)
            (let* ((id (slot-value node 'anatevka::id))
                   (address (process-public-address node))
                   (sprouted? (not (null (anatevka::blossom-node-match-edge node)))))
              (setf (gethash address (anatevka::dryad-ids dryad))       id
                    (gethash address (anatevka::dryad-sprouted? dryad)) sprouted?)))
          (simulation-add-event simulation (make-event :callback dryad))

          (simulate-until-dead simulation supervisor-A)
          (simulate-until-dead simulation supervisor-I)
          (simulate-until-dead simulation supervisor-M)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0 2)
                  :internal-weight 1
                  :children (list (vv-edge A B)))
               (B :id (id 2 2)
                  :children (list (vv-edge B C))
                  :internal-weight 1
                  :match-edge (vv-edge B C)
                  :parent (vv-edge B A)
                  :positive? nil)
               (C :id (id 4 2)
                  :internal-weight 1
                  :match-edge (vv-edge C B)
                  :parent (vv-edge C B))
               (D :id (id 2 0)
                  :internal-weight 1
                  :children (list (vv-edge D E)))
               (E :id (id 4 0)
                  :children (list (vv-edge E F))
                  :internal-weight 1
                  :match-edge (vv-edge E F)
                  :parent (vv-edge E D)
                  :positive? nil)
               (F :id (id 6 0)
                  :internal-weight 1
                  :match-edge (vv-edge F E)
                  :parent (vv-edge F E))
               (G :id (id 7 0)
                  :internal-weight 0
                  :match-edge (vv-edge G H)
                  :parent (vv-edge G H))
               (H :id (id 9 0)
                  :children (list (vv-edge H G))
                  :internal-weight 2
                  :match-edge (vv-edge H G)
                  :parent (vv-edge H I)
                  :positive? nil)
               (I :id (id 11 0)
                  :internal-weight 0
                  :children (list (vv-edge I H)))
               (J :id (id 9 2)
                  :internal-weight 0
                  :match-edge (vv-edge J K)
                  :parent (vv-edge J K))
               (K :id (id 11 2)
                  :children (list (vv-edge K J))
                  :internal-weight 2
                  :match-edge (vv-edge K J)
                  :parent (vv-edge K L)
                  :positive? nil)
               (L :id (id 13 2)
                  :internal-weight 0
                  :children (list (vv-edge L K)))
               (M :id (id 14 2)
                  :internal-weight 1
                  :children (list (vv-edge M N)))
               (N :id (id 16 2)
                  :children (list (vv-edge N O))
                  :internal-weight 1
                  :match-edge (vv-edge N O)
                  :parent (vv-edge N M)
                  :positive? nil)
               (O :id (id 18 2)
                  :internal-weight 1
                  :match-edge (vv-edge O N)
                  :parent (vv-edge O N))
               (P :id (id 16 0)
                  :internal-weight 1
                  :children (list (vv-edge P Q)))
               (Q :id (id 18 0)
                  :children (list (vv-edge Q R))
                  :internal-weight 1
                  :match-edge (vv-edge Q R)
                  :parent (vv-edge Q P)
                  :positive? nil)
               (R :id (id 20 0)
                  :internal-weight 1
                  :match-edge (vv-edge R Q)
                  :parent (vv-edge R Q)))
            (is (tree-equalp original-tree target-tree))))))))


(deftest test-supervisor-multireweight-4-clusters ()
  "Checks that this configuration will result in the following:
- one of the two clusters (A,D) and (I,L) will multireweight by 1
- one of the two clusters (M,P) and (V,Y) will multireweight by 1

 0  2  0        0  2  0   0  2  0        0  2  0
 +  -  +        +  -  +   +  -  +        +  -  +
 A->B=>C        J<=K<-L   M->N=>O        W<=X<-Y

    D->E=>F  G<=H<-I         P->Q=>R  S<=U<-V
    +  -  +  +  -  +         +  -  +  +  -  +
    0  2  0  0  2  0         0  2  0  0  2  0

d(B, D), d(H, J), d(N, P), d(U, W) = 2 and d(F, G), d(R, S) = 1 and d(L, M) = 2
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2)
              :children (list (vv-edge A B))
              :held-by-roots (list D)
              :paused? T)
           (B :id (id 2 2)
              :children (list (vv-edge B C))
              :internal-weight 2
              :match-edge (vv-edge B C)
              :parent (vv-edge B A)
              :positive? nil)
           (C :id (id 4 2)
              :match-edge (vv-edge C B)
              :parent (vv-edge C B))
           (D :id (id 2 0)
              :children (list (vv-edge D E))
              :held-by-roots (list A))
           (E :id (id 4 0)
              :children (list (vv-edge E F))
              :internal-weight 2
              :match-edge (vv-edge E F)
              :parent (vv-edge E D)
              :positive? nil)
           (F :id (id 6 0)
              :match-edge (vv-edge F E)
              :parent (vv-edge F E))
           (G :id (id 7 0)
              :match-edge (vv-edge G H)
              :parent (vv-edge G H))
           (H :id (id 9 0)
              :children (list (vv-edge H G))
              :internal-weight 2
              :match-edge (vv-edge H G)
              :parent (vv-edge H I)
              :positive? nil)
           (I :id (id 11 0)
              :children (list (vv-edge I H))
              :held-by-roots (list L)
              :paused? T)
           (J :id (id 9 2)
              :match-edge (vv-edge J K)
              :parent (vv-edge J K))
           (K :id (id 11 2)
              :children (list (vv-edge K J))
              :internal-weight 2
              :match-edge (vv-edge K J)
              :parent (vv-edge K L)
              :positive? nil)
           (L :id (id 13 2)
              :children (list (vv-edge L K))
              :held-by-roots (list I))
           (M :id (id 15 2)
              :children (list (vv-edge M N))
              :held-by-roots (list P)
              :paused? T)
           (N :id (id 17 2)
              :children (list (vv-edge N O))
              :internal-weight 2
              :match-edge (vv-edge N O)
              :parent (vv-edge N M)
              :positive? nil)
           (O :id (id 19 2)
              :match-edge (vv-edge O N)
              :parent (vv-edge O N))
           (P :id (id 17 0)
              :children (list (vv-edge P Q))
              :held-by-roots (list M))
           (Q :id (id 19 0)
              :children (list (vv-edge Q R))
              :internal-weight 2
              :match-edge (vv-edge Q R)
              :parent (vv-edge Q P)
              :positive? nil)
           (R :id (id 21 0)
              :match-edge (vv-edge R Q)
              :parent (vv-edge R Q))
           (S :id (id 22 0)
              :match-edge (vv-edge S U)
              :parent (vv-edge S U))
           (U :id (id 24 0)
              :children (list (vv-edge U S))
              :internal-weight 2
              :match-edge (vv-edge U S)
              :parent (vv-edge U V)
              :positive? nil)
           (V :id (id 26 0)
              :children (list (vv-edge V U))
              :held-by-roots (list Y)
              :paused? T)
           (W :id (id 24 2)
              :match-edge (vv-edge W X)
              :parent (vv-edge W X))
           (X :id (id 26 2)
              :children (list (vv-edge X W))
              :internal-weight 2
              :match-edge (vv-edge X W)
              :parent (vv-edge X Y)
              :positive? nil)
           (Y :id (id 28 2)
              :children (list (vv-edge Y X))
              :held-by-roots (list V)))
        (let ((supervisor-A (supervisor simulation
                                        :recommendation ':hold
                                        :weight 0
                                        :edges (list (vv-edge B D))
                                        :source-root (process-public-address A)
                                        :target-root (process-public-address D)
                                        :source-id (slot-value B 'anatevka::id)
                                        :root-bucket (list
                                                      (process-public-address D))))
              (supervisor-I (supervisor simulation
                                        :recommendation ':hold
                                        :weight 0
                                        :edges (list (vv-edge I K))
                                        :source-root (process-public-address I)
                                        :target-root (process-public-address L)
                                        :source-id (slot-value I 'anatevka::id)
                                        :root-bucket (list (process-public-address L))))
              (supervisor-M (supervisor simulation
                                        :recommendation ':hold
                                        :weight 0
                                        :edges (list (vv-edge N P))
                                        :source-root (process-public-address M)
                                        :target-root (process-public-address P)
                                        :source-id (slot-value N 'anatevka::id)
                                        :root-bucket (list
                                                      (process-public-address P))))
              (supervisor-V (supervisor simulation
                                        :recommendation ':hold
                                        :weight 0
                                        :edges (list (vv-edge V X))
                                        :source-root (process-public-address V)
                                        :target-root (process-public-address Y)
                                        :source-id (slot-value V 'anatevka::id)
                                        :root-bucket (list (process-public-address Y)))))
          (simulate-add-tree simulation original-tree)

          ;; fill the dryad and add it to the simulation
          (dolist (node original-tree)
            (let* ((id (slot-value node 'anatevka::id))
                   (address (process-public-address node))
                   (sprouted? (not (null (anatevka::blossom-node-match-edge node)))))
              (setf (gethash address (anatevka::dryad-ids dryad))       id
                    (gethash address (anatevka::dryad-sprouted? dryad)) sprouted?)))
          (simulation-add-event simulation (make-event :callback dryad))

          (simulate-until-dead simulation supervisor-A)
          (simulate-until-dead simulation supervisor-I)
          (simulate-until-dead simulation supervisor-M)
          (simulate-until-dead simulation supervisor-V)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0 2)
                  :internal-weight 1
                  :children (list (vv-edge A B)))
               (B :id (id 2 2)
                  :children (list (vv-edge B C))
                  :internal-weight 1
                  :match-edge (vv-edge B C)
                  :parent (vv-edge B A)
                  :positive? nil)
               (C :id (id 4 2)
                  :internal-weight 1
                  :match-edge (vv-edge C B)
                  :parent (vv-edge C B))
               (D :id (id 2 0)
                  :internal-weight 1
                  :children (list (vv-edge D E)))
               (E :id (id 4 0)
                  :children (list (vv-edge E F))
                  :internal-weight 1
                  :match-edge (vv-edge E F)
                  :parent (vv-edge E D)
                  :positive? nil)
               (F :id (id 6 0)
                  :internal-weight 1
                  :match-edge (vv-edge F E)
                  :parent (vv-edge F E))
               (G :id (id 7 0)
                  :internal-weight 0
                  :match-edge (vv-edge G H)
                  :parent (vv-edge G H))
               (H :id (id 9 0)
                  :children (list (vv-edge H G))
                  :internal-weight 2
                  :match-edge (vv-edge H G)
                  :parent (vv-edge H I)
                  :positive? nil)
               (I :id (id 11 0)
                  :internal-weight 0
                  :children (list (vv-edge I H)))
               (J :id (id 9 2)
                  :internal-weight 0
                  :match-edge (vv-edge J K)
                  :parent (vv-edge J K))
               (K :id (id 11 2)
                  :children (list (vv-edge K J))
                  :internal-weight 2
                  :match-edge (vv-edge K J)
                  :parent (vv-edge K L)
                  :positive? nil)
               (L :id (id 13 2)
                  :internal-weight 0
                  :children (list (vv-edge L K)))
               (M :id (id 15 2)
                  :internal-weight 0
                  :children (list (vv-edge M N)))
               (N :id (id 17 2)
                  :children (list (vv-edge N O))
                  :internal-weight 2
                  :match-edge (vv-edge N O)
                  :parent (vv-edge N M)
                  :positive? nil)
               (O :id (id 19 2)
                  :internal-weight 0
                  :match-edge (vv-edge O N)
                  :parent (vv-edge O N))
               (P :id (id 17 0)
                  :internal-weight 0
                  :children (list (vv-edge P Q)))
               (Q :id (id 19 0)
                  :children (list (vv-edge Q R))
                  :internal-weight 2
                  :match-edge (vv-edge Q R)
                  :parent (vv-edge Q P)
                  :positive? nil)
               (R :id (id 21 0)
                  :internal-weight 0
                  :match-edge (vv-edge R Q)
                  :parent (vv-edge R Q))
               (S :id (id 22 0)
                  :internal-weight 1
                  :match-edge (vv-edge S U)
                  :parent (vv-edge S U))
               (U :id (id 24 0)
                  :children (list (vv-edge U S))
                  :internal-weight 1
                  :match-edge (vv-edge U S)
                  :parent (vv-edge U V)
                  :positive? nil)
               (V :id (id 26 0)
                  :internal-weight 1
                  :children (list (vv-edge V U)))
               (W :id (id 24 2)
                  :internal-weight 1
                  :match-edge (vv-edge W X)
                  :parent (vv-edge W X))
               (X :id (id 26 2)
                  :children (list (vv-edge X W))
                  :internal-weight 1
                  :match-edge (vv-edge X W)
                  :parent (vv-edge X Y)
                  :positive? nil)
               (Y :id (id 28 2)
                  :internal-weight 1
                  :children (list (vv-edge Y X))))
            (is (tree-equalp original-tree target-tree))))))))
