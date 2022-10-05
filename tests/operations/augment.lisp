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
