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
