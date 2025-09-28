;;;; tests/operations/reweight.lisp
;;;;
;;;; Unit tests for the `REWEIGHT' supervisor action.

(in-package #:anatevka-tests)

(deftest test-supervisor-reweight-successfully ()
  "Checks the transformation

  A <== B <-- C    D --> E ==> F   -->   A <== B <-- C    D --> E ==> F
  +     -     +    +     -     +         +     -     +    +     -     +
  0     2     0    0     2     0         2     0     2    0     2     0

d(C, D) = 2
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 0)
              :match-edge (vv-edge A B)
              :parent (vv-edge A B))
           (B :id (id 2 0)
              :children (list (vv-edge B A))
              :internal-weight 2
              :parent (vv-edge B C)
              :match-edge (vv-edge B A)
              :positive? nil)
           (C :id (id 4 0)
              :children (list (vv-edge C B))
              :paused? T)
           (D :id (id 6 0)
              :children (list (vv-edge D E)))
           (E :id (id 8 0)
              :children (list (vv-edge E F))
              :internal-weight 2
              :match-edge (vv-edge E F)
              :parent (vv-edge E D)
              :positive? nil)
           (F :id (id 10 0)
              :match-edge (vv-edge F E)
              :parent (vv-edge F E)))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':augment
                                      :weight 2
                                      :edges (list (vv-edge C D))
                                      :source-root (process-public-address C)
                                      :target-root (process-public-address D)
                                      :source-id (slot-value C 'anatevka::id))))
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
              ((A :id (id 0 0)
                  :internal-weight 2
                  :match-edge (vv-edge A B)
                  :parent (vv-edge A B))
               (B :id (id 2 0)
                  :children (list (vv-edge B A))
                  :parent (vv-edge B C)
                  :match-edge (vv-edge B A)
                  :positive? nil)
               (C :id (id 4 0)
                  :internal-weight 2
                  :children (list (vv-edge C B)))
               (D :id (id 6 0)
                  :children (list (vv-edge D E)))
               (E :id (id 8 0)
                  :children (list (vv-edge E F))
                  :internal-weight 2
                  :match-edge (vv-edge E F)
                  :parent (vv-edge E D)
                  :positive? nil)
               (F :id (id 10 0)
                  :match-edge (vv-edge F E)
                  :parent (vv-edge F E)))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-reweight-rewind-simultaneous ()
  "Checks the transformation

  A <== B <-- C    D --> E ==> F   -->   A <== B <-- C    D --> E ==> F
  +     -     +    +     -     +         +     -     +    +     -     +
  0     2     0    0     2     0        3/2   1/2   3/2  3/2   1/2   3/2

d(C, D) = 3

The point of this test is to show that we can break livelock induced by
repeated reweighting and rewinding.
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 0)
              :match-edge (vv-edge A B)
              :parent (vv-edge A B))
           (B :id (id 2 0)
              :children (list (vv-edge B A))
              :internal-weight 2
              :parent (vv-edge B C)
              :match-edge (vv-edge B A)
              :positive? nil)
           (C :id (id 4 0)
              :children (list (vv-edge C B))
              :paused? T)
           (D :id (id 7 0)
              :children (list (vv-edge D E))
              :paused? T)
           (E :id (id 9 0)
              :internal-weight 2
              :children (list (vv-edge E F))
              :match-edge (vv-edge E F)
              :parent (vv-edge E D)
              :positive? nil)
           (F :id (id 11 0)
              :match-edge (vv-edge F E)
              :parent (vv-edge F E)))
        (let ((supervisor-left (supervisor simulation
                                           :recommendation ':contract
                                           :weight 2
                                           :edges (list (vv-edge C A))
                                           :source-root (process-public-address C)
                                           :target-root (process-public-address C)
                                           :source-id (slot-value C 'anatevka::id)))
              (supervisor-right (supervisor simulation
                                            :recommendation ':contract
                                            :weight 2
                                            :edges (list (vv-edge D F))
                                            :source-root (process-public-address D)
                                            :target-root (process-public-address D)
                                            :source-id (slot-value D 'anatevka::id))))
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
              ((A :id (id 0 0)
                  :internal-weight 3/2
                  :match-edge (vv-edge A B)
                  :parent (vv-edge A B))
               (B :id (id 2 0)
                  :children (list (vv-edge B A))
                  :internal-weight 1/2
                  :parent (vv-edge B C)
                  :match-edge (vv-edge B A)
                  :positive? nil)
               (C :id (id 4 0)
                  :internal-weight 3/2
                  :children (list (vv-edge C B)))
               (D :id (id 7 0)
                  :internal-weight 3/2
                  :children (list (vv-edge D E)))
               (E :id (id 9 0)
                  :internal-weight 1/2
                  :children (list (vv-edge E F))
                  :match-edge (vv-edge E F)
                  :parent (vv-edge E D)
                  :positive? nil)
               (F :id (id 11 0)
                  :internal-weight 3/2
                  :match-edge (vv-edge F E)
                  :parent (vv-edge F E)))
            (is (tree-equalp original-tree target-tree))))))))
