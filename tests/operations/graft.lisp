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
