;;;; tests/operations/contract.lisp
;;;;
;;;; Unit tests for the `CONTRACT' supervisor action.

(in-package #:anatevka-tests)

(deftest test-supervisor-contract-3-blossom-minimal ()
  "Checks the transformation

                                     -
 +     -     +                    BLOSSOM
 A --> B ==> C        -->     [A --- B --- C]
                              [\-----------/]
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0) :children (list (vv-edge A B))
              :internal-weight 2 :paused? T)
           (B :id (id 2) :parent (vv-edge B A)        :children (list (vv-edge B C))
              :match-edge (vv-edge B C)
              :positive? nil)
           (C :id (id 4) :parent (vv-edge C B)        :internal-weight 2
              :match-edge (vv-edge C B)))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':contract
                                      :edges (list (vv-edge A C))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address A)
                                      :source-id (id 0))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (let* ((finished-blossom (dereference (slot-value A 'anatevka::pistil)))
                 (finished-tree (list* finished-blossom original-tree)))
            (blossom-let (target-tree :dryad dryad-address)
                ((BLOSSOM :id (slot-value finished-blossom 'anatevka::id)
                          :dryad nil
                          :petals (list (vv-edge A C)
                                        (vv-edge C B)
                                        (vv-edge B A)))
                 (A :id (id 0) :pistil BLOSSOM :internal-weight 2)
                 (B :id (id 2) :pistil BLOSSOM)
                 (C :id (id 4) :pistil BLOSSOM :internal-weight 2))
              (is (tree-equalp finished-tree target-tree)))))))))

(deftest test-supervisor-contract-3-blossom-duff ()
  "Checks the transformation

                                                           +
 +     -     +     -     +              +     -         BLOSSOM
 A --> B ==> C --> D ==> E      -->     A --> B ==> [C --- D --- E]
                                                    [\-----------/]
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0) :children (list (vv-edge A B))
              :internal-weight 2 :paused? T)
           (B :id (id 2) :parent (vv-edge B A)          :children (list (vv-edge B C))
              :match-edge (vv-edge B C)                 :positive? nil)
           (C :id (id 4) :parent (vv-edge C B)          :children (list (vv-edge C D))
              :match-edge (vv-edge C B)                 :internal-weight 2)
           (D :id (id 6) :parent (vv-edge D C)          :children (list (vv-edge D E))
              :match-edge (vv-edge D E)                 :positive? nil)
           (E :id (id 8) :parent (vv-edge E D)          :internal-weight 2
              :match-edge (vv-edge E D)))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':contract
                                      :edges (list (vv-edge C E))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address A)
                                      :source-id (slot-value C 'anatevka::id))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (let* ((finished-blossom (dereference (slot-value C 'anatevka::pistil)))
                 (finished-tree (list* finished-blossom original-tree)))
            (blossom-let (target-tree :dryad dryad-address)
                ((BLOSSOM :id (slot-value finished-blossom 'anatevka::id)
                          :dryad nil
                          :petals (list (vv-edge C E)
                                        (vv-edge E D)
                                        (vv-edge D C))
                          :match-edge (bb-edge BLOSSOM C B B)
                          :parent (bb-edge BLOSSOM C B B))
                 (A :id (id 0) :children (list (vv-edge A B)) :internal-weight 2)
                 (B :id (id 2) :parent (vv-edge B A)
                    :children (list (bb-edge B B C BLOSSOM))
                    :match-edge (bb-edge B B C BLOSSOM)
                    :positive? nil)
                 (C :id (id 4) :pistil BLOSSOM              :internal-weight 2)
                 (D :id (id 6) :pistil BLOSSOM)
                 (E :id (id 8) :pistil BLOSSOM              :internal-weight 2))
              (is (tree-equalp finished-tree target-tree)))))))))

(deftest test-supervisor-contract-3-blossom-treetop ()
  "Checks the transformation

                                               +
 +     -     +     -     +                  BLOSSOM         -     +
 A --> B ==> C --> D ==> E      -->     [A --- B --- C] --> D ==> E
                                        [\-----------/]
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0) :children (list (vv-edge A B))
              :internal-weight 2 :paused? T)
           (B :id (id 2) :parent (vv-edge B A)          :children (list (vv-edge B C))
              :match-edge (vv-edge B C)                 :positive? nil)
           (C :id (id 4) :parent (vv-edge C B)          :children (list (vv-edge C D))
              :match-edge (vv-edge C B)                 :internal-weight 2)
           (D :id (id 6) :parent (vv-edge D C)          :children (list (vv-edge D E))
              :match-edge (vv-edge D E)                 :positive? nil)
           (E :id (id 8) :parent (vv-edge E D)          :internal-weight 2
              :match-edge (vv-edge E D)))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':contract
                                      :edges (list (vv-edge A C))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address A)
                                      :source-id (slot-value A 'anatevka::id))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (let* ((finished-blossom (dereference
                                    (slot-value A 'anatevka::pistil)))
                 (finished-tree (list* finished-blossom original-tree)))
            (blossom-let (target-tree :dryad dryad-address)
                ((BLOSSOM :id (slot-value finished-blossom 'anatevka::id)
                          :dryad nil
                          :petals (list (vv-edge A C)
                                        (vv-edge C B)
                                        (vv-edge B A))
                          :children (list (bb-edge BLOSSOM C D D)))
                 (A :id (id 0) :pistil BLOSSOM       :internal-weight 2)
                 (B :id (id 2) :pistil BLOSSOM)
                 (C :id (id 4) :pistil BLOSSOM       :internal-weight 2)
                 (D :id (id 6) :positive? nil        :match-edge (vv-edge D E)
                    :parent (bb-edge D D C BLOSSOM)
                    :children (list (vv-edge D E)))
                 (E :id (id 8) :parent (vv-edge E D) :match-edge (vv-edge E D)
                    :internal-weight 2))
              (is (tree-equalp finished-tree target-tree)))))))))

(deftest test-supervisor-contract-5-blossom-minimal ()
  "Checks the transformation

                     +
  -     +         BLOSSOM
  B ==> C        [B --- C]
  ^              [|     |]
  |              [|     |]
+ A         -->  [A     |]
  |              [|     |]
  v              [|     |]
  D ==> E        [D --- E]
  -     +
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2) :children (list (vv-edge A B))
              :internal-weight 2 :paused? T)
           (B :id (id 0 4) :parent (vv-edge B A)          :children (list (vv-edge B C))
              :match-edge (vv-edge B C)                   :positive? nil)
           (C :id (id 2 4) :parent (vv-edge C B)          :match-edge (vv-edge C B)
              :internal-weight 2)
           (D :id (id 0 0) :parent (vv-edge D A)          :children (list (vv-edge D E))
              :match-edge (vv-edge D E)                   :positive? nil)
           (E :id (id 2 0) :parent (vv-edge E D)          :internal-weight 2
              :match-edge (vv-edge E D)))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':contract
                                      :edges (list (vv-edge C E))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address A)
                                      :source-id (slot-value C 'anatevka::id))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (let* ((finished-blossom (dereference
                                    (slot-value A 'anatevka::pistil)))
                 (finished-tree (list* finished-blossom original-tree)))
            (blossom-let (target-tree :dryad dryad-address)
                ((BLOSSOM :id (slot-value finished-blossom 'anatevka::id)
                          :dryad nil
                          :petals (list (vv-edge A B)
                                        (vv-edge B C)
                                        (vv-edge C E)
                                        (vv-edge E D)
                                        (vv-edge D A)))
                 (A :id (id 0 2) :pistil BLOSSOM  :internal-weight 2)
                 (B :id (id 0 4) :pistil BLOSSOM)
                 (C :id (id 2 4) :pistil BLOSSOM  :internal-weight 2)
                 (D :id (id 0 0) :pistil BLOSSOM)
                 (E :id (id 2 0) :pistil BLOSSOM  :internal-weight 2))
              (is (tree-equalp finished-tree target-tree)))))))))

(deftest test-supervisor-contract-5-blossom-duff ()
  "Checks the transformation

                                             +
              -     +                     BLOSSOM
              D ==> E                    [D --- E]
              ^                          [|     |]
  +     -     |              +     -     [|     |]
  A --> B ==> C +       -->  A --> B ==> [C     |]
              |                          [|     |]
              v                          [|     |]
              G ==> F                    [G --- F]
              -     +
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2) :children (list (vv-edge A B))
              :internal-weight 2 :paused? T)
           (B :id (id 2 2) :parent (vv-edge B A)          :match-edge (vv-edge B C)
              :positive? nil                              :children (list (vv-edge B C)))
           (C :id (id 4 2) :parent (vv-edge C B)          :match-edge (vv-edge C B)
              :children (list (vv-edge C D) (vv-edge C G))
              :internal-weight 2)
           (D :id (id 4 4) :parent (vv-edge D C)          :children (list (vv-edge D E))
              :match-edge (vv-edge D E)                   :positive? nil)
           (E :id (id 6 4) :parent (vv-edge E D)          :match-edge (vv-edge E D)
              :internal-weight 2)
           (F :id (id 6 0) :parent (vv-edge F G)          :match-edge (vv-edge F G)
              :internal-weight 2)
           (G :id (id 4 0) :parent (vv-edge G C)          :children (list (vv-edge G F))
              :match-edge (vv-edge G F)     :positive? nil))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':contract
                                      :edges (list (vv-edge E F))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address A)
                                      :source-id (slot-value E 'anatevka::id))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (let* ((finished-blossom (dereference
                                    (slot-value E 'anatevka::pistil)))
                 (finished-tree (list* finished-blossom original-tree)))
            (blossom-let (target-tree :dryad dryad-address)
                ((BLOSSOM :id (slot-value finished-blossom 'anatevka::id)
                          :dryad nil
                          :parent (bb-edge BLOSSOM C B B)
                          :match-edge (bb-edge BLOSSOM C B B)
                          :petals (list (vv-edge C D)
                                        (vv-edge D E)
                                        (vv-edge E F)
                                        (vv-edge F G)
                                        (vv-edge G C)))
                 (A :id (id 0 2) :children (list (vv-edge A B)) :internal-weight 2)
                 (B :id (id 2 2) :parent (vv-edge B A)
                    :match-edge (bb-edge B B C BLOSSOM)
                    :positive? nil
                    :children (list (bb-edge B B C BLOSSOM)))
                 (C :id (id 4 2) :pistil BLOSSOM  :internal-weight 2)
                 (D :id (id 4 4) :pistil BLOSSOM)
                 (E :id (id 6 4) :pistil BLOSSOM  :internal-weight 2)
                 (F :id (id 6 0) :pistil BLOSSOM  :internal-weight 2)
                 (G :id (id 4 0) :pistil BLOSSOM))
              (is (tree-equalp finished-tree target-tree)))))))))

(deftest test-supervisor-contract-5-blossom-treetop ()
  "Checks the transformation

                                +
  -     +                    BLOSSOM
  B ==> C                   [B --- C]
  ^                         [|     |]
  |                         [|     |]
+ A                    -->  [A     |]
  |                         [|     |]
  v                         [|     |]
  D ==> E --> F ==> G       [D --- E] --> F ==> G
  -     +     -     +                     -     +
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*)))
              (with-address-dereferencing ()))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2) :children (list (vv-edge A B))
              :internal-weight 2 :paused? T)
           (B :id (id 0 4) :parent (vv-edge B A)          :children (list (vv-edge B C))
              :match-edge (vv-edge B C)                   :positive? nil)
           (C :id (id 2 4) :parent (vv-edge C B)          :match-edge (vv-edge C B)
              :internal-weight 2)
           (D :id (id 0 0) :parent (vv-edge D A)          :children (list (vv-edge D E))
              :match-edge (vv-edge D E)                   :positive? nil)
           (E :id (id 2 0) :parent (vv-edge E D)          :internal-weight 2
              :match-edge (vv-edge E D)                   :children (list (vv-edge E F)))
           (F :id (id 4 0) :parent (vv-edge F E)          :children (list (vv-edge F G))
              :match-edge (vv-edge F G)                   :positive? nil)
           (G :id (id 6 0) :parent (vv-edge G F)          :internal-weight 2
              :match-edge (vv-edge G F)))
        (let ((supervisor (supervisor simulation
                                      :recommendation ':contract
                                      :edges (list (vv-edge C E))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address A)
                                      :source-id (slot-value C 'anatevka::id))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (let* ((finished-blossom (dereference
                                    (slot-value A 'anatevka::pistil)))
                 (finished-tree (list* finished-blossom original-tree)))
            (blossom-let (target-tree :dryad dryad-address)
                ((BLOSSOM :id (slot-value finished-blossom 'anatevka::id)
                          :dryad nil
                          :children (list (bb-edge BLOSSOM E F F))
                          :petals (list (vv-edge A B)
                                        (vv-edge B C)
                                        (vv-edge C E)
                                        (vv-edge E D)
                                        (vv-edge D A)))
                 (A :id (id 0 2) :pistil BLOSSOM  :internal-weight 2)
                 (B :id (id 0 4) :pistil BLOSSOM)
                 (C :id (id 2 4) :pistil BLOSSOM  :internal-weight 2)
                 (D :id (id 0 0) :pistil BLOSSOM)
                 (E :id (id 2 0) :pistil BLOSSOM  :internal-weight 2)
                 (F :id (id 4 0) :parent (bb-edge F F E BLOSSOM)
                    :match-edge (vv-edge F G)
                    :positive? nil
                    :children (list (vv-edge F G)))
                 (G :id (id 6 0) :parent (vv-edge G F)
                    :internal-weight 2
                    :match-edge (vv-edge G F)))
              (is (tree-equalp finished-tree target-tree)))))))))
