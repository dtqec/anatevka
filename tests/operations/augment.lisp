;;;; tests/operations/augment.lisp
;;;;
;;;; Unit tests for the `AUGMENT' supervisor action.

(in-package #:anatevka-tests)

(deftest test-supervisor-augment-sapling-sapling ()
  "Checks the transformation

+     +                +     +
A ~~~ B       -->      A === B"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*))))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0) :internal-weight 2 :paused? T)
           (B :id (id 2)))
        (let ((supervisor (supervisor simulation
                                      :edges (list (vv-edge A B))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address B)
                                      :recommendation ':augment
                                      :source-id (id 0))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((left  :id (id 0) :match-edge (vv-edge left right)
                      :internal-weight 2)
               (right :id (id 2) :match-edge (vv-edge right left)))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-augment-sapling-treetop ()
  "Checks the transformation

+     +     -     +               +     +     +     +
A ~~~ B --> C ==> D      -->      A === B     C === D"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*))))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0)  :internal-weight 2 :paused? T)
           (B :id (id 2)
              :children (list (vv-edge B C)))
           (C :id (id 4) :internal-weight 2
              :match-edge (vv-edge C D)       :parent (vv-edge C B)
              :children (list (vv-edge C D)) :positive? nil)
           (D :id (id 6)
              :match-edge (vv-edge D C)     :parent (vv-edge D C)))
        (let ((supervisor (supervisor simulation
                                      :edges (list (vv-edge A B))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address B)
                                      :recommendation ':augment
                                      :source-id (id 0))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0) :match-edge (vv-edge A B) :internal-weight 2)
               (B :id (id 2) :match-edge (vv-edge B A))
               (C :id (id 4) :match-edge (vv-edge C D) :internal-weight 2)
               (D :id (id 6) :match-edge (vv-edge D C)))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-augment-sapling-duff ()
  "Checks the transformation

+     +     -     +               +     +     +     +
A ~~~ D <== C <-- B      -->      A === D     C === B"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*))))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0) 
              :internal-weight 2
              :paused? T)
           (B :id (id 6)
              :children (list (vv-edge B C)))
           (C :id (id 4)
              :internal-weight 2
              :children (list (vv-edge C D))
              :parent (vv-edge C B)
              :match-edge (vv-edge C D)
              :positive? nil)
           (D :id (id 2)
              :match-edge (vv-edge D C)
              :parent (vv-edge D C)))
        (let ((supervisor (supervisor simulation
                                      :edges (list (vv-edge A D))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address B)
                                      :recommendation ':augment
                                      :source-id (id 0))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0) :match-edge (vv-edge A D) :internal-weight 2)
               (B :id (id 6) :match-edge (vv-edge B C))
               (C :id (id 4) :match-edge (vv-edge C B) :internal-weight 2)
               (D :id (id 2) :match-edge (vv-edge D A)))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-augment-root-has-two-children ()
  "Checks the transformation

+     +     -     +     -     +              +     +     +     +     +     +
A ~~~ D <== C <-- B --> E ==> F     -->      A === D     C === B     E === F"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*))))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0)
              :internal-weight 2
              :paused? T)
           (B :id (id 6)
              :children (list (vv-edge B C) (vv-edge B E)))
           (C :id (id 4)
              :internal-weight 2
              :children (list (vv-edge C D))
              :parent (vv-edge C B)
              :match-edge (vv-edge C D)
              :positive? nil)
           (D :id (id 2)
              :match-edge (vv-edge D C)
              :parent (vv-edge D C))
           (E :id (id 8)
              :internal-weight 2
              :children (list (vv-edge E F))
              :parent (vv-edge E B)
              :match-edge (vv-edge E F)
              :positive? nil)
           (F :id (id 10)
              :match-edge (vv-edge F E)
              :parent (vv-edge F E))
           )
        (let ((supervisor (supervisor simulation
                                      :edges (list (vv-edge A D))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address B)
                                      :recommendation ':augment
                                      :source-id (id 0))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0) :match-edge (vv-edge A D) :internal-weight 2)
               (B :id (id 6) :match-edge (vv-edge B C))
               (C :id (id 4) :match-edge (vv-edge C B) :internal-weight 2)
               (D :id (id 2) :match-edge (vv-edge D A))
               (E :id (id 8) :match-edge (vv-edge E F) :internal-weight 2)
               (F :id (id 10) :match-edge (vv-edge F E))
               )
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-augment-3-blossom-minimal ()
  "Checks the transformation

       +                                +
    BLOSSOM        +      --->       BLOSSOM         +
[A --- B --- C]    D             [A --- B --- C] === D
[\-----------/]                  [\-----------/]
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((BLOSSOM :id 'blossom
                    :dryad dryad-address
                    :petals (list (vv-edge A C)
                                  (vv-edge C B)
                                  (vv-edge B A)))
           (A :id (id 0) :pistil BLOSSOM :internal-weight 2)
           (B :id (id 2) :pistil BLOSSOM)
           (C :id (id 4) :pistil BLOSSOM :internal-weight 2)
           (D :id (id 6) :paused? T))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':augment
                                      :edges (list (bb-edge D D C BLOSSOM))
                                      :source-root (process-public-address D)
                                      :target-root (process-public-address BLOSSOM)
                                      :source-id (id 6))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((BLOSSOM :id 'blossom
                        :dryad dryad-address
                        :petals (list (vv-edge A C)
                                      (vv-edge C B)
                                      (vv-edge B A))
                        :match-edge (bb-edge BLOSSOM C D D))
               (A :id (id 0) :pistil BLOSSOM :internal-weight 2)
               (B :id (id 2) :pistil BLOSSOM)
               (C :id (id 4) :pistil BLOSSOM :internal-weight 2)
               (D :id (id 6) :match-edge (bb-edge D D C BLOSSOM)))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-augment-3-blossom-minimal-reverse ()
  "Checks the transformation

       +                                +
    BLOSSOM        +      --->       BLOSSOM         +
[A --- B --- C]    D             [A --- B --- C] === D
[\-----------/]                  [\-----------/]
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((BLOSSOM :id 'blossom
                    :dryad dryad-address
                    :petals (list (vv-edge A C)
                                  (vv-edge C B)
                                  (vv-edge B A))
                    :paused? T)
           (A :id (id 0) :pistil BLOSSOM :internal-weight 2)
           (B :id (id 2) :pistil BLOSSOM)
           (C :id (id 4) :pistil BLOSSOM :internal-weight 2)
           (D :id (id 6)))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':augment
                                      :edges (list (bb-edge BLOSSOM C D D))
                                      :source-root (process-public-address BLOSSOM)
                                      :target-root (process-public-address D)
                                      :source-id (id 4))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((BLOSSOM :id 'blossom
                        :dryad dryad-address
                        :petals (list (vv-edge A C)
                                      (vv-edge C B)
                                      (vv-edge B A))
                        :match-edge (bb-edge BLOSSOM C D D))
               (A :id (id 0) :pistil BLOSSOM :internal-weight 2)
               (B :id (id 2) :pistil BLOSSOM)
               (C :id (id 4) :pistil BLOSSOM :internal-weight 2)
               (D :id (id 6) :match-edge (bb-edge D D C BLOSSOM)))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-augment-3-blossom-root ()
  "Checks the transformation

       +                                            +
    BLOSSOM         -     +     +      --->      BLOSSOM         +     +     +
[A --- B --- C] --> D ==> E ~~~ F            [A --- B --- C] === D     E === F
[\-----------/]                              [\-----------/]
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((BLOSSOM :id 'blossom
                    :dryad dryad-address
                    :petals (list (vv-edge A C)
                                  (vv-edge C B)
                                  (vv-edge B A))
                    :children (list (bb-edge BLOSSOM C D D)))
           (A :id (id 0) :pistil BLOSSOM :internal-weight 2)
           (B :id (id 2) :pistil BLOSSOM)
           (C :id (id 4) :pistil BLOSSOM :internal-weight 2)
           (D :id (id 6) :parent (bb-edge D D C BLOSSOM) :match-edge (vv-edge D E)
              :children (list (vv-edge D E)) :positive? nil)
           (E :id (id 8) :internal-weight 2 :match-edge (vv-edge E D)
              :parent (vv-edge E D))
           (F :id (id 10) :paused? T))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':augment
                                      :edges (list (vv-edge F E))
                                      :source-root (process-public-address F)
                                      :target-root (process-public-address BLOSSOM)
                                      :source-id (id 10))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((BLOSSOM :id 'blossom
                        :dryad dryad-address
                        :petals (list (vv-edge A C)
                                      (vv-edge C B)
                                      (vv-edge B A))
                        :match-edge (bb-edge BLOSSOM C D D))
               (A :id (id 0) :pistil BLOSSOM :internal-weight 2)
               (B :id (id 2) :pistil BLOSSOM)
               (C :id (id 4) :pistil BLOSSOM :internal-weight 2)
               (D :id (id 6) :match-edge (bb-edge D D C BLOSSOM))
               (E :id (id 8) :internal-weight 2 :match-edge (vv-edge E F))
               (F :id (id 10) :match-edge (vv-edge F E))
               )
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-augment-3-blossom-root-reverse ()
  "Checks the transformation

       +                                            +
    BLOSSOM         -     +     +      --->      BLOSSOM         +     +     +
[A --- B --- C] --> D ==> E ~~~ F            [A --- B --- C] === D     E === F
[\-----------/]                              [\-----------/]
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((BLOSSOM :id 'blossom
                    :dryad dryad-address
                    :petals (list (vv-edge A C)
                                  (vv-edge C B)
                                  (vv-edge B A))
                    :children (list (bb-edge BLOSSOM C D D))
                    :paused? T)
           (A :id (id 0) :pistil BLOSSOM :internal-weight 2)
           (B :id (id 2) :pistil BLOSSOM)
           (C :id (id 4) :pistil BLOSSOM :internal-weight 2)
           (D :id (id 6) :parent (bb-edge D D C BLOSSOM) :match-edge (vv-edge D E)
              :children (list (vv-edge D E)) :positive? nil)
           (E :id (id 8) :internal-weight 2 :match-edge (vv-edge E D)
              :parent (vv-edge E D))
           (F :id (id 10)))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':augment
                                      :edges (list (vv-edge E F))
                                      :source-root (process-public-address BLOSSOM)
                                      :target-root (process-public-address F)
                                      :source-id (id 8))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((BLOSSOM :id 'blossom
                        :dryad dryad-address
                        :petals (list (vv-edge A C)
                                      (vv-edge C B)
                                      (vv-edge B A))
                        :match-edge (bb-edge BLOSSOM C D D))
               (A :id (id 0) :pistil BLOSSOM :internal-weight 2)
               (B :id (id 2) :pistil BLOSSOM)
               (C :id (id 4) :pistil BLOSSOM :internal-weight 2)
               (D :id (id 6) :match-edge (bb-edge D D C BLOSSOM))
               (E :id (id 8) :internal-weight 2 :match-edge (vv-edge E F))
               (F :id (id 10) :match-edge (vv-edge F E))
               )
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-augment-3-blossom-root-two-children ()
  "Checks the transformation

                  +                                                   +
+     -        BLOSSOM        -     +     +   --->   +     +       BLOSSOM        +     +     +
H <== G <-- [A -- B -- C] --> D ==> E ~~~ F          H === G    [A -- B -- C] === D     E === F
            [\---------/]                                       [\---------/]
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((BLOSSOM :id 'blossom
                    :dryad dryad-address
                    :petals (list (vv-edge A C)
                                  (vv-edge C B)
                                  (vv-edge B A))
                    :children (list (bb-edge BLOSSOM C D D)
                                    (bb-edge BLOSSOM A G G)))
           (H :id (id 0) :internal-weight 2 :parent (vv-edge H G)
              :match-edge (vv-edge H G))
           (G :id (id 2) :parent (bb-edge G G A BLOSSOM) :match-edge (vv-edge G H)
              :positive? nil :children (list (vv-edge G H)))
           (A :id (id 4) :pistil BLOSSOM :internal-weight 2)
           (B :id (id 6) :pistil BLOSSOM)
           (C :id (id 8) :pistil BLOSSOM :internal-weight 2)
           (D :id (id 10) :parent (bb-edge D D C BLOSSOM) :match-edge (vv-edge D E)
              :children (list (vv-edge D E)) :positive? nil)
           (E :id (id 12) :internal-weight 2 :match-edge (vv-edge E D)
              :parent (vv-edge E D))
           (F :id (id 14) :paused? T))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':augment
                                      :edges (list (vv-edge F E))
                                      :source-root (process-public-address F)
                                      :target-root (process-public-address BLOSSOM)
                                      :source-id (id 14))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((BLOSSOM :id 'blossom
                        :dryad dryad-address
                        :petals (list (vv-edge A C)
                                      (vv-edge C B)
                                      (vv-edge B A))
                        :match-edge (bb-edge BLOSSOM C D D))
               (H :id (id 0) :internal-weight 2 :match-edge (vv-edge H G))
               (G :id (id 2) :match-edge (vv-edge G H))
               (A :id (id 4) :pistil BLOSSOM :internal-weight 2)
               (B :id (id 6) :pistil BLOSSOM)
               (C :id (id 8) :pistil BLOSSOM :internal-weight 2)
               (D :id (id 10) :match-edge (bb-edge D D C BLOSSOM))
               (E :id (id 12) :internal-weight 2 :match-edge (vv-edge E F))
               (F :id (id 14) :match-edge (vv-edge F E))
               )
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-augment-3-blossom-root-two-children-reverse ()
  "Checks the transformation

                  +                                                   +
+     -        BLOSSOM        -     +     +   --->   +     +       BLOSSOM        +     +     +
H <== G <-- [A -- B -- C] --> D ==> E ~~~ F          H === G    [A -- B -- C] === D     E === F
            [\---------/]                                       [\---------/]
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((BLOSSOM :id 'blossom
                    :dryad dryad-address
                    :petals (list (vv-edge A C)
                                  (vv-edge C B)
                                  (vv-edge B A))
                    :children (list (bb-edge BLOSSOM C D D)
                                    (bb-edge BLOSSOM A G G))
                    :paused? T)
           (H :id (id 0) :internal-weight 2 :parent (vv-edge H G)
              :match-edge (vv-edge H G))
           (G :id (id 2) :parent (bb-edge G G A BLOSSOM) :match-edge (vv-edge G H)
              :positive? nil :children (list (vv-edge G H)))
           (A :id (id 4) :pistil BLOSSOM :internal-weight 2)
           (B :id (id 6) :pistil BLOSSOM)
           (C :id (id 8) :pistil BLOSSOM :internal-weight 2)
           (D :id (id 10) :parent (bb-edge D D C BLOSSOM) :match-edge (vv-edge D E)
              :children (list (vv-edge D E)) :positive? nil)
           (E :id (id 12) :internal-weight 2 :match-edge (vv-edge E D)
              :parent (vv-edge E D))
           (F :id (id 14)))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':augment
                                      :edges (list (vv-edge E F))
                                      :source-root (process-public-address BLOSSOM)
                                      :target-root (process-public-address F)
                                      :source-id (id 12))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((BLOSSOM :id 'blossom
                        :dryad dryad-address
                        :petals (list (vv-edge A C)
                                      (vv-edge C B)
                                      (vv-edge B A))
                        :match-edge (bb-edge BLOSSOM C D D))
               (H :id (id 0) :internal-weight 2 :match-edge (vv-edge H G))
               (G :id (id 2) :match-edge (vv-edge G H))
               (A :id (id 4) :pistil BLOSSOM :internal-weight 2)
               (B :id (id 6) :pistil BLOSSOM)
               (C :id (id 8) :pistil BLOSSOM :internal-weight 2)
               (D :id (id 10) :match-edge (bb-edge D D C BLOSSOM))
               (E :id (id 12) :internal-weight 2 :match-edge (vv-edge E F))
               (F :id (id 14) :match-edge (vv-edge F E))
               )
            (is (tree-equalp original-tree target-tree))))))))
