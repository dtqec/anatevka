;;;; tests/dryad.lisp
;;;;
;;;; Tests dryad functionality: in particular the expansion of macrovertices at
;;;; the termination of the blossom algorithm.

(in-package #:anatevka-tests)

(deftest test-dryad-expand-barbell-3-blossom ()
  "Tests that a DRYAD will properly execute the following blossom expansion:

        +
     BLOSSOM         +             +     +     +     +
 [A --- B --- C] === D     -->     A === B     C === D
 [\-----------/]
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*))))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (simulation-add-event simulation (make-event :callback dryad))
      (blossom-let (original-tree :dryad dryad-address)
          ((BLOSSOM :id (gensym "BLOSSOM")
                    :match-edge (bb-edge BLOSSOM C D D)
                    :petals (list (vv-edge A C)
                                  (vv-edge C B)
                                  (vv-edge B A)))
           (A :id (id 0) :internal-weight 2 :pistil BLOSSOM)
           (B :id (id 2) :pistil BLOSSOM)
           (C :id (id 4) :internal-weight 2 :pistil BLOSSOM)
           (D :id (id 6) :match-edge (bb-edge D D C BLOSSOM)))
        (simulate-add-tree simulation original-tree :dryad dryad)
        (simulate-until-dead simulation BLOSSOM :timeout 100)
        (blossom-let (target-tree :dryad dryad-address)
            ((BLOSSOM-new :id (slot-value BLOSSOM 'anatevka::id)
                      :match-edge (bb-edge BLOSSOM-new C D D)
                      :petals (list (vv-edge A C)
                                    (vv-edge C B)
                                    (vv-edge B A))
                      :wilting t)
             (A :id (id 0) :match-edge (vv-edge A B)
                :internal-weight 2)
             (B :id (id 2) :match-edge (vv-edge B A))
             (C :id (id 4) :match-edge (vv-edge C D)
                :internal-weight 2)
             (D :id (id 6) :match-edge (vv-edge D C)))
          (is (tree-equalp original-tree target-tree)))))))

(deftest test-dryad-expand-barbell-3-blossom-x2 ()
  "Tests that a DRYAD will properly execute the following blossom expansion:

        +                   +
     BLOSSOM             BLOSSOM2                +     +     +     +     +     +
 [A --- B --- C] === [D --- E --- F]     -->     A === B     C === D     E === F
 [\-----------/]     [\-----------/]
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*))))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (simulation-add-event simulation (make-event :callback dryad))
      (blossom-let (original-tree :dryad dryad-address)
          ((BLOSSOM :id (gensym "BLOSSOM-A")
                    :match-edge (bb-edge BLOSSOM C D BLOSSOM2)
                    :petals (list (vv-edge A C)
                                  (vv-edge C B)
                                  (vv-edge B A)))
           (BLOSSOM2 :id (gensym "BLOSSOM-B")
                     :match-edge (bb-edge BLOSSOM2 D C BLOSSOM)
                     :petals (list (vv-edge D F)
                                   (vv-edge F E)
                                   (vv-edge E D)))
           (A :id (id 0) :internal-weight 2 :pistil BLOSSOM)
           (B :id (id 2) :pistil BLOSSOM)
           (C :id (id 4) :internal-weight 2 :pistil BLOSSOM)
           (D :id (id 6) :pistil BLOSSOM2)
           (E :id (id 8) :internal-weight 2 :pistil BLOSSOM2)
           (F :id (id 10) :pistil BLOSSOM2)
           )
        (simulate-add-tree simulation original-tree :dryad dryad)
        (simulate-until-dead simulation BLOSSOM :timeout 100)
        (simulate-until-dead simulation BLOSSOM2 :timeout 100)
        (blossom-let (target-tree :dryad dryad-address)
            ((BLOSSOM-new :id (slot-value BLOSSOM 'anatevka::id)
                          :match-edge (bb-edge BLOSSOM-new C D D)
                          :petals (list (vv-edge A C)
                                        (vv-edge C B)
                                        (vv-edge B A))
                          :wilting t)
             ;; nb: the match-edge here still has the other macrovertex in it
             ;;     which probably means that BLOSSOM2 was popped first
             (BLOSSOM2-new :id (slot-value BLOSSOM2 'anatevka::id)
                           :match-edge (bb-edge BLOSSOM2-new D C BLOSSOM-new)
                           :petals (list (vv-edge D F)
                                         (vv-edge F E)
                                         (vv-edge E D))
                           :wilting t
                           )
             (A :id (id 0) :match-edge (vv-edge A B)
                :internal-weight 2)
             (B :id (id 2) :match-edge (vv-edge B A))
             (C :id (id 4) :match-edge (vv-edge C D)
                :internal-weight 2)
             (D :id (id 6) :match-edge (vv-edge D C))
             (E :id (id 8) :internal-weight 2 :match-edge (vv-edge E F))
             (F :id (id 10) :match-edge (vv-edge F E))
             )
          (is (tree-equalp original-tree target-tree)))))))

(deftest test-dryad-expand-barbell-5-blossom ()
  "Tests that a DRYAD will properly execute the following blossom expansion:

     +
  BLOSSOM              +     +
 [B --- C]             B === C
 [|     |]
 [|     |]
 [A     |]        -->  A +
 [|     |]             !
 [|     |]             !     +     +
 [D --- E] === F     + D     E === F
"
  (with-with ((with-courier ())
              (with-simulation (simulation (*local-courier*))))
    (let* ((dryad (spawn-process 'dryad :match-address (register)
                                        :debug? t))
           (dryad-address (process-public-address dryad)))
      (simulation-add-event simulation (make-event :callback dryad))
      (blossom-let (original-tree :dryad dryad-address)
          ((BLOSSOM :id (gensym "BLOSSOM")
                    :match-edge (bb-edge BLOSSOM E F F)
                    :petals (list (vv-edge A D)
                                  (vv-edge D E)
                                  (vv-edge E C)
                                  (vv-edge C B)
                                  (vv-edge B A)))
           (A :id (id 0 2) :internal-weight 2 :pistil BLOSSOM)
           (B :id (id 0 4) :pistil BLOSSOM)
           (C :id (id 2 4) :internal-weight 2 :pistil BLOSSOM)
           (D :id (id 0 0) :pistil BLOSSOM)
           (E :id (id 2 0) :internal-weight 2 :pistil BLOSSOM)
           (F :id (id 4 0) :match-edge (bb-edge F F E BLOSSOM)))
        (simulate-add-tree simulation original-tree :dryad dryad)
        (simulate-until-dead simulation BLOSSOM :timeout 100)
        (blossom-let (target-tree :dryad dryad-address)
            ((BLOSSOM-new :id (slot-value BLOSSOM 'anatevka::id)
                          :match-edge (bb-edge BLOSSOM-new E F F)
                          :petals (list (vv-edge A D)
                                        (vv-edge D E)
                                        (vv-edge E C)
                                        (vv-edge C B)
                                        (vv-edge B A))
                          :wilting t)
             (A :id (id 0 2) :match-edge (vv-edge A D)
                :internal-weight 2)
             (B :id (id 0 4) :match-edge (vv-edge B C))
             (C :id (id 2 4) :match-edge (vv-edge C B)
                :internal-weight 2)
             (D :id (id 0 0) :match-edge (vv-edge D A))
             (E :id (id 2 0) :match-edge (vv-edge E F)
                :internal-weight 2)
             (F :id (id 4 0) :match-edge (vv-edge F E)))
          (is (tree-equalp original-tree target-tree)))))))
