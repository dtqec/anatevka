;;;; tests/operations/graft.lisp
;;;;
;;;; Unit tests for the `GRAFT' supervisor action.

(in-package #:anatevka-tests)

(deftest test-supervisor-graft-sapling ()
  "Checks the transformation

+     +    +                +     -     +
A ~~~ B == C       -->      A --> B ==> C"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*))))
    (blossom-let (original-tree)
        ((sapling    :id (id 0) :internal-weight 2 :paused? T)
         (left-node  :id (id 2) :match-edge (vv-edge left-node right-node))
         (right-node :id (id 4) :internal-weight 2
                     :match-edge (vv-edge right-node left-node)))
      (let ((supervisor (supervisor simulation
                                    :edges (list (vv-edge left-node right-node)
                                                 (vv-edge sapling left-node))
                                    :source-root (process-public-address sapling)
                                    :target-root (process-public-address left-node)
                                    :recommendation ':graft
                                    :source-id (id 0))))
        (simulate-add-tree simulation original-tree)
        (simulate-until-dead simulation supervisor)
        (blossom-let (target-tree)
            ((root :id (id 0) :internal-weight 2
                   :children (list (vv-edge root inner-node)))
             (inner-node :id (id 2)
                         :match-edge (vv-edge inner-node outer-node)
                         :parent (vv-edge inner-node root)
                         :children (list (vv-edge inner-node outer-node))
                         :positive? nil)
             (outer-node :id (id 4) :internal-weight 2
                         :match-edge (vv-edge outer-node inner-node)
                         :parent (vv-edge outer-node inner-node)))
          (is (tree-equalp original-tree target-tree)))))))

(deftest test-supervisor-graft-inner-3-blossom ()
  "Checks the transformation

              +                            -
           BLOSSOM      +               BLOSSOM      +
          [      D] === E              [      D] ==> E
          [    / |]         -->        [    / |]
          [  /   |]                    [  /   |]
    A     [B --- C]              A --> [B --- C]
    +      +     +               +      -     -
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*))))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0) :paused? T)
           (B :id (id 2) :pistil BLOSSOM
              :internal-weight 2)
           (C :id (id 4) :pistil BLOSSOM)
           (D :id (id 4 2) :pistil BLOSSOM
              :internal-weight 2)
           (E :id (id 6 2)
              :match-edge (bb-edge E E D BLOSSOM))
           (BLOSSOM :id 'blossom
                    :petals (list (vv-edge B C)
                                  (vv-edge C D)
                                  (vv-edge D B))
                    :match-edge (bb-edge BLOSSOM D E E)))
        (let ((supervisor (supervisor simulation
                                      :edges (list (bb-edge BLOSSOM D E E)
                                                   (bb-edge A A B BLOSSOM))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address BLOSSOM)
                                      :recommendation ':graft
                                      :source-id (id 0))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0) :children (list (bb-edge A A B BLOSSOM)))
               (B :id (id 2) :pistil BLOSSOM
                  :internal-weight 2)
               (C :id (id 4) :pistil BLOSSOM)
               (D :id (id 4 2) :pistil BLOSSOM
                  :internal-weight 2)
               (E :id (id 6 2)
                  :parent (bb-edge E E D BLOSSOM)
                  :match-edge (bb-edge E E D BLOSSOM))
               (BLOSSOM :id 'blossom
                        :petals (list (vv-edge B C)
                                      (vv-edge C D)
                                      (vv-edge D B))
                        :parent (bb-edge BLOSSOM B A A)
                        :match-edge (bb-edge BLOSSOM D E E)
                        :positive? nil
                        :children (list (bb-edge BLOSSOM D E E))))
            (is (tree-equalp original-tree target-tree))))))))
