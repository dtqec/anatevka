;;;; tests/operations/expand.lisp
;;;;
;;;; Unit tests for the `EXPAND' supervisor action.
;;;;
;;;; NOTE: Supervisors expand inner blossoms only -- dryads handle barbells.

(in-package #:anatevka-tests)

(deftest test-supervisor-expand-inner-3-blossom-match/=parent ()
  "Checks the transformation

              -
           BLOSSOM       +                  -       +
          [      D] ==> E                    D ==> E
          [    / |]         -->              ^
          [  /   |]                          |
    A --> [B --- C]              A --> B ==> C
   +                             +     -      +
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*))))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0)     :children (list (bb-edge A A B BLOSSOM))
              :paused? T)
           (B :id (id 2)     :pistil BLOSSOM
              :positive? nil :internal-weight 2)
           (C :id (id 4)     :pistil BLOSSOM
              :positive? nil)
           (D :id (id 4 2)   :pistil BLOSSOM
              :positive? nil :internal-weight 2)
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
        (let ((supervisor (supervisor simulation
                                      :edges (list (bb-edge BLOSSOM BLOSSOM
                                                            nil nil))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address A)
                                      :recommendation ':expand
                                      :source-id (id 0))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0)
                  :children (list (vv-edge A B)))
               (B :id (id 2) :internal-weight 2 :positive? nil
                  :parent (vv-edge B A) :match-edge (vv-edge B C)
                  :children (list (vv-edge B C)))
               (C :id (id 4)
                  :parent (vv-edge C B) :match-edge (vv-edge C B)
                  :children (list (vv-edge C D)))
               (D :id (id 4 2) :internal-weight 2 :positive? nil
                  :parent (vv-edge D C) :match-edge (vv-edge D E)
                  :children (list (vv-edge D E)))
               (E :id (id 6 2)
                  :parent (vv-edge E D) :match-edge (vv-edge E D))
               (BLOSSOM :id 'blossom :dryad dryad-address
                        :petals (list (vv-edge B C)
                                      (vv-edge C D)
                                      (vv-edge D B))
                        :parent (bb-edge BLOSSOM B A A)
                        :match-edge (bb-edge BLOSSOM D E E)
                        :positive? nil
                        :children (list (bb-edge BLOSSOM D E E))
                        :wilting T))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-expand-inner-3-blossom-match=parent ()
  "Checks the transformation (! means match in vertical direction)

              -
           BLOSSOM
          [      D]                     D +
          [    / |]                     !
          [  /   |]         +      -    !
    A --> [B --- C]   -->   A --> B     C +
   +       !                      !
           v                      v
           E                      E
            +                      +
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*))))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2)
              :children (list (bb-edge A A B BLOSSOM))
              :paused? T)
           (B :id (id 2 2)   :pistil BLOSSOM
              :positive? nil :internal-weight 2)
           (C :id (id 4 2)   :pistil BLOSSOM
              :positive? nil)
           (D :id (id 4 4)   :pistil BLOSSOM
              :positive? nil :internal-weight 2)
           (E :id (id 2 0)
              :parent (bb-edge E E B BLOSSOM)
              :match-edge (bb-edge E E B BLOSSOM))
           (BLOSSOM :id 'blossom
                    :petals (list (vv-edge B C)
                                  (vv-edge C D)
                                  (vv-edge D B))
                    :parent (bb-edge BLOSSOM B A A)
                    :match-edge (bb-edge BLOSSOM B E E)
                    :positive? nil
                    :children (list (bb-edge BLOSSOM B E E))))
        (let ((supervisor (supervisor simulation
                                      :edges (list (bb-edge BLOSSOM BLOSSOM
                                                            nil nil))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address A)
                                      :recommendation ':expand
                                      :source-id (id 0 2))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          (blossom-let (target-tree :dryad dryad-address)
              ;; A --> B ==> E
              ;; C === D
              ((A :id (id 0 2)
                  :children (list (vv-edge A B)))
               (B :id (id 2 2) :internal-weight 2 :positive? nil
                  :match-edge (vv-edge B E)
                  :parent (vv-edge B A) :children (list (vv-edge B E)))
               (C :id (id 4 2)
                  :match-edge (vv-edge C D))
               (D :id (id 4 4) :internal-weight 2
                  :match-edge (vv-edge D C))
               (E :id (id 2 0)
                  :match-edge (vv-edge E B)
                  :parent (vv-edge E B))
               (BLOSSOM :id 'blossom
                        :petals (list (vv-edge B C)
                                      (vv-edge C D)
                                      (vv-edge D B))
                        :parent (bb-edge BLOSSOM B A A)
                        :match-edge (bb-edge BLOSSOM B E E)
                        :positive? nil
                        :children (list (bb-edge BLOSSOM B E E))
                        :wilting T))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-expand-inner-5-blossom-match/=parent ()
  "Checks the transformation (! means match in vertical direction)

              -
           BLOSSOM       +             +       -      +
          [F --- E] ==> G               F ---> E ==> G
          [|     |]                     ^
   +      [|     |]                     !
    A --> [B     |]         -->   A --> B
          [|     |]              +       -
          [|     |]
          [C --- D]                   + C === D +
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*))))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2)
              :children (list (bb-edge A A B BLOSSOM))
              :paused? T)
           (B :id (id 2 2) :pistil BLOSSOM :positive? nil :internal-weight 2)
           (C :id (id 2 0) :pistil BLOSSOM :positive? nil)
           (D :id (id 4 0) :pistil BLOSSOM :positive? nil :internal-weight 2)
           (E :id (id 4 4) :pistil BLOSSOM :positive? nil :internal-weight 2)
           (F :id (id 2 4) :pistil BLOSSOM :positive? nil)
           (G :id (id 6 4)
              :parent (bb-edge G G E BLOSSOM)
              :match-edge (bb-edge G G E BLOSSOM))
           (BLOSSOM :id 'blossom
                    :petals (list (vv-edge B C)
                                  (vv-edge C D)
                                  (vv-edge D E)
                                  (vv-edge E F)
                                  (vv-edge F B))
                    :parent (bb-edge BLOSSOM B A A)
                    :match-edge (bb-edge BLOSSOM E G G)
                    :positive? nil
                    :children (list (bb-edge BLOSSOM E G G))))
        (let ((supervisor (supervisor simulation
                                      :edges (list (bb-edge BLOSSOM BLOSSOM
                                                            nil nil))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address A)
                                      :recommendation ':expand
                                      :source-id (id 0 2))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          ;; A --> B ==> F --> E ==> G
          ;; C === D
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0 2)          :children (list (vv-edge A B)))
               (B :id (id 2 2)          :match-edge (vv-edge B F)
                  :parent (vv-edge B A) :children (list (vv-edge B F))
                  :internal-weight 2    :positive? nil)
               (C :id (id 2 0)          :match-edge (vv-edge C D))
               (D :id (id 4 0)          :match-edge (vv-edge D C)
                  :internal-weight 2)
               (E :id (id 4 4)          :match-edge (vv-edge E G)
                  :parent (vv-edge E F) :children (list (vv-edge E G))
                  :internal-weight 2    :positive? nil)
               (F :id (id 2 4)          :match-edge (vv-edge F B)
                  :parent (vv-edge F B) :children (list (vv-edge F E)))
               (G :id (id 6 4)          :match-edge (vv-edge G E)
                  :parent (vv-edge G E))
               (BLOSSOM :id 'blossom
                        :petals (list (vv-edge B C)
                                      (vv-edge C D)
                                      (vv-edge D E)
                                      (vv-edge E F)
                                      (vv-edge F B))
                        :parent (bb-edge BLOSSOM B A A)
                        :match-edge (bb-edge BLOSSOM E G G)
                        :positive? nil
                        :children (list (bb-edge BLOSSOM E G G))
                        :wilting T))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-expand-inner-5-blossom-match=parent ()
  "Checks the transformation (! means match in vertical direction)

              -
           BLOSSOM                     +       +
          [F --- E]                     F === E
          [|     |]
          [|     |]
          [|     D]         -->               D +
          [|     |]                           !
          [|     |]              +       -    !
    A --> [B --- C]               A --> B     C +
   +       !                            !
           v                            v
           G +                          G +
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*))))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address)
          ((A :id (id 0 2)
              :children (list (bb-edge A A B BLOSSOM))
              :paused? T)
           (B :id (id 2 2) :pistil BLOSSOM :positive? nil :internal-weight 2)
           (C :id (id 4 2) :pistil BLOSSOM :positive? nil)
           (D :id (id 4 4) :pistil BLOSSOM :positive? nil :internal-weight 2)
           (E :id (id 4 6) :pistil BLOSSOM :positive? nil)
           (F :id (id 2 6) :pistil BLOSSOM :positive? nil :internal-weight 2)
           (G :id (id 2 0)
              :parent (bb-edge G G B BLOSSOM)
              :match-edge (bb-edge G G B BLOSSOM))
           (BLOSSOM :id 'blossom
                    :petals (list (vv-edge B C)
                                  (vv-edge C D)
                                  (vv-edge D E)
                                  (vv-edge E F)
                                  (vv-edge F B))
                    :parent (bb-edge BLOSSOM B A A)
                    :match-edge (bb-edge BLOSSOM B G G)
                    :positive? nil
                    :children (list (bb-edge BLOSSOM B G G))))
        (let ((supervisor (supervisor simulation
                                      :edges (list (bb-edge BLOSSOM BLOSSOM
                                                            nil nil))
                                      :source-root (process-public-address A)
                                      :target-root (process-public-address A)
                                      :recommendation ':expand
                                      :source-id (id 0 2))))
          (simulate-add-tree simulation original-tree)
          (simulate-until-dead simulation supervisor)
          ;; A --> B ==> G
          ;; C === D
          ;; F === E
          (blossom-let (target-tree :dryad dryad-address)
              ((A :id (id 0 2) 
                  :children (list (vv-edge A B)))
               (B :id (id 2 2) :match-edge (vv-edge B G) :internal-weight 2
                  :parent (vv-edge B A) :children (list (vv-edge B G))
                  :positive? nil)
               (C :id (id 4 2) :match-edge (vv-edge C D))
               (D :id (id 4 4) :match-edge (vv-edge D C) :internal-weight 2)
               (E :id (id 4 6) :match-edge (vv-edge E F))
               (F :id (id 2 6) :match-edge (vv-edge F E) :internal-weight 2)
               (G :id (id 2 0) :match-edge (vv-edge G B)
                  :parent (vv-edge G B))
               (BLOSSOM :id 'blossom
                        :petals (list (vv-edge B C)
                                      (vv-edge C D)
                                      (vv-edge D E)
                                      (vv-edge E F)
                                      (vv-edge F B))
                        :parent (bb-edge BLOSSOM B A A)
                        :match-edge (bb-edge BLOSSOM B G G)
                        :positive? nil
                        :children (list (bb-edge BLOSSOM B G G))
                        :wilting T))
            (is (tree-equalp original-tree target-tree))))))))

(deftest test-supervisor-expand-blossom-blossom-barbell ()
  "Checks the transformation (! means match in vertical direction)

 BLOSSOM1      BLOSSOM2                       BLOSSOM2
[A --- B] === [D --- E]         A     B  === [D --- E]
[|   /  ]     [|   /  ]         !            [|   /  ]
[|  /   ]     [|  /   ]     --> !            [|  /   ]
[| /    ]     [| /    ]         !            [| /    ]
[C      ]     [F      ]         C            [F      ]
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*))))
    (let ((dryad-address (register)))
      (blossom-let (original-tree :dryad dryad-address
                                  :positive? nil)
          ((A :id (id 0 0) :pistil BLOSSOM1)
           (B :id (id 2 0) :pistil BLOSSOM1 :internal-weight 2)
           (C :id (id 0 2) :pistil BLOSSOM1 :internal-weight 2)
           (D :id (id 4 0) :pistil BLOSSOM2)
           (E :id (id 6 0) :pistil BLOSSOM2 :internal-weight 2)
           (F :id (id 4 2) :pistil BLOSSOM2 :internal-weight 2)
           (BLOSSOM1 :id 'blossom1
                     :petals (list (vv-edge A B)
                                   (vv-edge B C)
                                   (vv-edge C A))
                     :match-edge (bb-edge BLOSSOM1 B D BLOSSOM2)
                     :positive? t)
           (BLOSSOM2 :id 'blossom2
                     :petals (list (vv-edge D E)
                                   (vv-edge E F)
                                   (vv-edge F D))
                     :match-edge (bb-edge BLOSSOM2 D B BLOSSOM1)
                     :positive? t))
        (send-message (process-public-address BLOSSOM1)
                      (anatevka::make-message-expand))
        (simulate-add-tree simulation original-tree)
        (simulate-until-dead simulation BLOSSOM1)
        (blossom-let (target-tree :dryad dryad-address)
            ((A :id (id 0 0) :positive? t
                :match-edge (vv-edge A C))
             (B :id (id 2 0) :positive? t :internal-weight 2
                :match-edge (bb-edge B B D BLOSSOM2))
             (C :id (id 0 2) :positive? t :internal-weight 2
                :match-edge (vv-edge C A))
             (D :id (id 4 0) :pistil BLOSSOM2 :positive? nil)
             (E :id (id 6 0) :pistil BLOSSOM2 :positive? nil :internal-weight 2)
             (F :id (id 4 2) :pistil BLOSSOM2 :positive? nil :internal-weight 2)
             (BLOSSOM1 :id 'blossom1
                       :petals (list (vv-edge A B)
                                     (vv-edge B C)
                                     (vv-edge C A))
                       :match-edge (bb-edge BLOSSOM1 B D BLOSSOM2)
                       :positive? t
                       :wilting t)
             (BLOSSOM2 :id 'blossom2
                       :petals (list (vv-edge D E)
                                     (vv-edge E F)
                                     (vv-edge F D))
                       :match-edge (bb-edge BLOSSOM2 D B B)
                       :positive? t))
          (is (tree-equalp original-tree target-tree)))))))
