;;;; operations/scan.lisp
;;;;
;;;; Commands related to scanning.
;;;;
;;;; a SCAN is the procedure internal to a blossom tree which queries its
;;;; children and its neighbors in order to calculate a suggested 'next step'
;;;; for the blossom algorithm.  a legal 'next step' is required to have a
;;;; 'minimality' property in the following senses:
;;;;
;;;;  (1) each proposed operation on the tree corresponds to an edge in the graph.
;;;;  (2) an operation which modifies the structure of a tree can only be
;;;;      performed along an edge whose 'adjusted weight' is zero.
;;;;  (3) a tree can also change its 'internal weights', which has the effect of
;;;;      (properly) decreasing the adjusted weights of all edges that might
;;;;      propose operations.
;;;;  (4) it is illegal (and signals a failure of correctness of the algorithm)
;;;;      for a proposing edge to have negative adjusted weight.
;;;;
;;;; taking these in concert, the goal of SCAN is to walk over all participating
;;;; edges, extract from them their proposed operations, and discern which
;;;; proposal minimally adjusts the internal weights, so as not to violate (4).
;;;;
;;;; to adhere to locality properties, this calculation is spread out over the
;;;; tree: each node forwards SCAN directives to its children and sends PINGs
;;;; out to its own neighbors, aggregating the replies by retaining the proposal
;;;; which minimally adjusts the internal weights.

(in-package #:anatevka)

;;;
;;; message definitions
;;;

(defstruct (message-ping (:include message))
  "Broadcast from one vertex to another, to probe its weight and its position in its tree.  Expects as a reply a PONG message.  Includes as part of its payload some information about the tree structure at the originator, so that the receiver can generate an appropriate recommendation."
  (root           nil :type (or null address))
  (blossom        nil :type (or null address))
  (id             nil :type t)
  (weight         nil :type real)
  (internal-roots nil :type list))

(defstruct (message-soft-ping (:include message-ping))
  "The same as a PING, but with the signaled intent that it _will not_ be used to advocate for a fresh tree operation, only to check the consistency of the state.")

(defstruct (message-pong (:include message))
  "A reply to a PING message, which contains the adjusted edge weight and its proposed tree operation.

Most of the slots are self-explanatory.  As an exception, ROOT-BUCKET aggregates values of TARGET-ROOT when two PONGs are merged by UNIFY-PONG."
  (weight         0      :type real)
  (edges          nil    :type list)
  (source-root    nil    :type (or null address))
  (target-root    nil    :type (or null address))
  (recommendation ':pass :type keyword)
  (source-id      nil    :type t)
  (root-bucket    nil    :type list))

(defstruct (message-adjoin-root (:include message))
  "A capsule used to move a PING around inside its target tree, which is done in order to accrue information (viz., the target blossom, root, and blossom weight) used to construct the reply PONG."
  (match-edge nil :type (or null blossom-edge))
  (ping       nil :type message-ping)
  (pong       nil :type message-pong)
  (positive?  nil :type boolean))

(defstruct (message-soft-adjoin-root (:include message-adjoin-root))
  "The same as an ADJOIN-ROOT, but with the signaled intent that it _will not_ be used to advocate for a fresh tree operation, only to check the consistency of the state.")

(defstruct (message-scan (:include message))
  "Broadcast internally to a tree, like activation down a the length of a neuron, to spur its vertices to send out PING messages to their neighbors."
  (local-root     nil :type (or null address))
  (local-blossom  nil :type (or null address))
  (weight         nil :type real)
  (internal-roots nil :type list)
  (strategy       nil :type discover-strategy))

(defstruct (message-soft-scan (:include message-scan))
  "The same as a SCAN, but it generates soft PINGs.")

(defmethod print-object ((pong message-pong) stream)
  "Convenience printing for PONGs."
  (print-unreadable-object (pong stream :type t :identity nil)
    (format stream "@~a "  (message-reply-channel pong))
    (format stream "RECOMMENDATION=~a " (message-pong-recommendation pong))
    (format stream "WEIGHT=~a " (message-pong-weight pong))
    (format stream "EDGES=~a " (message-pong-edges pong))
    (format stream "SOURCE-ID=~a " (message-pong-source-id pong))
    (format stream "SOURCE-ROOT=~a " (message-pong-source-root pong))
    (format stream "TARGET-ROOT=~a " (message-pong-target-root pong))
    (format stream "ROOT-BUCKET=~a " (message-pong-root-bucket pong))
    (format stream "~a" (aether::message-message-id pong))))

;;;
;;; message operations
;;;

(defmethod make-pong ((node blossom-node) &rest initargs)
  "Wrapper for generating a pong message of the appropriate type for `NODE'."
  (apply #'make-message-pong initargs))

(defmethod make-supervisor ((node blossom-node) &rest initargs)
  "Wrapper for generating a supervisor of the appropriate type for `NODE'."
  (apply #'spawn-process 'supervisor initargs))

(defmethod unify-pongs (x (y message-pong) &key internal-root-set)
  "Given two PONGs proposing two operations, selects the more immediate of the two.

Returns a VALUES pair of the unified PONG message as well as a boolean indicating whether the default clause was triggered, for other method handlers to decide whether a preceding handler has made a decision that they are then overriding, or if there's no danger of overriding an intentional decision.

When INTERNAL-ROOT-SET is supplied, discard HOLD recommendations which emanate from the indicated root addresses."
  ;; IMPORTANT NOTE: Textbook descriptions of the forest version of the blossom
  ;;     algorithm may give the impression that _all_ operations proposed by
  ;;     nonpositive nodes should be ignored, including those in foreign trees.
  ;;     This is not so: the reweighting operation must consider all foreign
  ;;     nodes, regardless of their positivity.
  (check-type internal-root-set list)
  (when (null x)
    (return-from unify-pongs
      y))
  (with-slots ((x-rec recommendation) (x-weight weight) (x-root target-root)) x
    (with-slots ((y-rec recommendation) (y-weight weight) (y-root target-root)) y
      (cond
        ;; drop `PASS'es
        ((eql ':pass x-rec)
         y)
        ((eql ':pass y-rec)
         x)
        ;; drop internal `HOLD's
        ((and (eql ':hold x-rec)
              (member x-root internal-root-set :test #'address=))
         y)
        ((and (eql ':hold y-rec)
              (member y-root internal-root-set :test #'address=))
         x)
        ;; if we're both (`HOLD' 0)ing, aggregate the root-bucket of each pong
        ((and (eql ':hold x-rec) (eql ':hold y-rec)
              (zerop x-weight) (zerop y-weight))
         (initialize-and-return ((pong (copy-message-pong x)))
           (setf (message-pong-root-bucket pong)
                 (remove-duplicates (union (message-pong-root-bucket x)
                                           (message-pong-root-bucket y))
                                    :test #'address=))))
        ;; (`HOLD' 0) is an expensive operation: either we idle or we have to
        ;; coordinate across multiple trees.  prefer easier actions.
        ((and (eql ':hold x-rec)
              (zerop x-weight) (zerop y-weight))
         y)
        ((and (eql ':hold y-rec)
              (zerop x-weight) (zerop y-weight))
         x)
        ;; finally, prefer lighter pongs to heavier ones
        ((< x-weight y-weight)
         (values x t))
        (t 
         (values y t))))))

(defun pong= (stale-pong replica-pong)
  "Checks whether two PONGs recommend the same operation."
  (and (every (lambda (fs)
                (destructuring-bind (f g) fs
                  (funcall f
                           (funcall g stale-pong)
                           (funcall g replica-pong))))
              `((,#'eql ,#'message-pong-recommendation)
                (,#'=   ,#'message-pong-weight)
                (,#'eql ,(a:compose #'aether::address-channel #'message-pong-source-root))
                (,#'eql ,(a:compose #'aether::address-channel #'message-pong-target-root))
                (,#'=   ,(a:compose #'length #'message-pong-edges))))
       (loop :for stale-edge :in (message-pong-edges stale-pong)
             :for replica-edge :in (message-pong-edges replica-pong)
             :always (every (lambda (fs)
                              (destructuring-bind (f g) fs
                                (funcall f
                                         (funcall g stale-edge)
                                         (funcall g replica-edge))))
                            `((,#'eql ,(a:compose #'aether::address-channel #'blossom-edge-source-vertex))
                              (,#'eql ,(a:compose #'aether::address-channel #'blossom-edge-target-vertex))
                              (,#'eql ,(a:compose #'aether::address-channel #'blossom-edge-source-node))
                              (,#'eql ,(a:compose #'aether::address-channel #'blossom-edge-target-node)))))))

;;;
;;; blossom-node data frame
;;;

(defstruct data-frame-scan
  "Data frame associated to a SCAN procedure running on a BLOSSOM process."
  (pong           nil :type message-pong)
  (soft?          nil :type boolean)
  (local-root     nil :type address)
  (local-blossom  nil :type address)
  (local-weight   nil :type (or null real))
  (vertices       nil :type list)
  (children       nil :type list)
  (petals         nil :type list)
  (weight         nil :type real)
  (internal-roots nil :type list)
  (strategy       nil :type discover-strategy))

;;;
;;; blossom-node command definitions
;;;

(define-process-upkeep ((node blossom-node)) (START-SCAN scan-message)
  "Sets up the scanning procedure stack frames."
  ;; if the node is wilting, we don't want it to start a scan, because it's
  ;; going to reach out to the dryad to get a list of addresses to ping, but
  ;; the dryad will have already excised this node from its lookup table
  (when (blossom-node-wilting node)
    ;; but, if this scan was initiated by someone other than this node,
    ;; we don't want them to sit around waiting forever, so let them know
    (a:when-let ((reply-channel (message-reply-channel scan-message)))
      ;; we can't use FINISH-SCAN without setting up the data frame,
      ;; so we elect to send the message directly from START-SCAN
      (send-message reply-channel (make-pong node)))
    (finish-handler))
  ;; load data frame
  (let* ((local-root (or (slot-value scan-message 'local-root)
                         (process-public-address node)))
         (local-blossom (if (and (blossom-node-pistil node)
                                 (slot-value scan-message 'local-blossom))
                            (slot-value scan-message 'local-blossom)
                            (process-public-address node)))
         (soft? (typep scan-message 'message-soft-scan))
         (weight (if (blossom-node-pistil node)
                     (- (slot-value scan-message 'weight)
                        (blossom-node-internal-weight node))
                     (- (blossom-node-internal-weight node))))
         ;; if we're a negative topmost blossom node, then we initialize the
         ;; pong with an EXPAND recommendation, as this is the only operation
         ;; that we can sponsor.
         (pong (if (and (null (blossom-node-pistil node))
                        (not (blossom-node-positive? node))
                        (blossom-node-petals node))
                   (make-pong node :recommendation ':expand
                                   :weight (blossom-node-internal-weight node)
                                   :edges (list (make-blossom-edge
                                                 :source-node local-blossom
                                                 :source-vertex (process-public-address node)))
                                   :source-root local-root
                                   :target-root local-root)
                   (make-pong node :target-root local-root)))
         ;; if we're a positive node, then we should forward the scan to both
         ;; our tree children and our blossom children (petals). if we're a
         ;; negative node, we forward just to our tree children.
         (forwarding-addresses (mapcar #'blossom-edge-target-node
                                       (if (blossom-node-positive? node)
                                           (union (blossom-node-petals node)
                                                  (blossom-node-children node))
                                           (blossom-node-children node))))
         (internal-roots (or (message-scan-internal-roots scan-message)
                             (list local-root)))
         (strategy (slot-value scan-message 'strategy)))
    (log-entry :entry-type ':starting-scan
               :deweight weight)
    (push (make-data-frame-scan :local-root local-root
                                :local-blossom local-blossom
                                :weight weight
                                :pong pong
                                :soft? soft?
                                :internal-roots internal-roots
                                :strategy strategy)
          (process-data-stack node))
    ;; load (most of) script
    (process-continuation node
                          `(CONTACT-DRYAD)
                          `(FORWARD-SCAN ,forwarding-addresses)
                          `(FINISH-SCAN ,(message-reply-channel scan-message)))))

(define-process-upkeep ((node blossom-node)) (CONTACT-DRYAD)
  "Request from the dryad responsible for this node a list of candidate node neighbors with which to coordinate for this node's next operation.  Defers the actual processing of that list to PROCESS-ADDRESSES."
  (unless (or (blossom-node-petals node)
              (not (blossom-node-positive? node)))
    (with-slots (weight strategy) (peek (process-data-stack node))
      (sync-rpc (make-message-discover
                 :id (blossom-node-id node)
                 :address (process-public-address node)
                 ;; negated bc `weight' is negated above in `START-SCAN' let block
                 :internal-weight (- weight)
                 :strategy strategy)
          (discovery-message (blossom-node-dryad node)
           :message-type message-discovery :message-unpacker identity
           :returned? returned?)
        (cond
          ((and returned? (blossom-node-wilting node))
           nil)
          (returned?
           (error "Live node got an RTS during dryad communication."))
          (t
           (process-continuation node `(PROCESS-ADDRESSES ,discovery-message))))))))

(define-process-upkeep ((node blossom-node)) (PROCESS-ADDRESSES discovery-message)
  "Performs postprocessing, if any, on the list of candidate neighbor nodes received from the dryad, then sets up the PING command to start communicating with them."
  (with-slots (channels-to-try) discovery-message
    (process-continuation node `(PING ,channels-to-try))))

(define-process-upkeep ((node blossom-node)) (FORWARD-SCAN addresses)
  "Sends a SCAN message to each of the children tabulated in ADDRESSES."
  (with-slots (local-root local-blossom weight pong soft? internal-roots strategy)
      (peek (process-data-stack node))
    (unless (blossom-node-wilting node)
      (flet ((payload-constructor ()
               (cond
                 (soft?
                  (make-message-soft-scan
                   :local-root local-root
                   :local-blossom local-blossom
                   :weight weight
                   :internal-roots internal-roots
                   :strategy strategy))
                 (t
                  (make-message-scan
                   :local-root local-root
                   :local-blossom local-blossom
                   :weight weight
                   :internal-roots internal-roots
                   :strategy strategy)))))
        (with-replies (replies
                       :returned? returned?
                       :message-type message-pong
                       :message-unpacker identity)
                      (send-message-batch #'payload-constructor addresses)
          (loop :for reply :in replies
                :unless (null reply)
                  :do (setf pong (unify-pongs pong reply :internal-root-set internal-roots))))))))

(define-process-upkeep ((node blossom-node)) (PING vertices)
  "Sends a PING message to any neighboring vertices as previously provided by the DISCOVER query sent to the dryad.

NOTE: this command is only installed when NODE is a vertex."
  (with-slots (local-root weight soft? pong local-blossom internal-roots)
      (peek (process-data-stack node))
    (unless (blossom-node-wilting node)
      (flet ((payload-constructor ()
               (cond
                 (soft?
                  (make-message-soft-ping
                   :root local-root
                   :blossom local-blossom
                   :weight weight
                   :id (blossom-node-id node)
                   :internal-roots internal-roots))
                 (t
                  (make-message-ping
                   :root local-root
                   :blossom local-blossom
                   :weight weight
                   :id (blossom-node-id node)
                   :internal-roots internal-roots)))))
        (log-entry :entry-type ':pinging-vertices
                   :log-level 1
                   :vertices vertices)
        (with-replies (replies
                       :returned? returned?
                       :message-type message-pong
                       :message-unpacker identity)
                      (send-message-batch #'payload-constructor vertices)
          (loop :for vertex :in vertices
                :for reply :in replies
                :unless (null reply)
                  :do (when (vertex? node)
                        (let ((edge (car (last (message-pong-edges reply)))))
                          (setf (blossom-edge-source-vertex edge) (process-public-address node)
                                (blossom-edge-source-node edge) local-blossom)))
                      (let ((reply-pong-rec (message-pong-recommendation reply))
                            (reply-pong-weight (message-pong-weight reply))
                            (reply-pong-edges (message-pong-edges reply))
                            (reply-pong-target (message-pong-target-root reply)))
                        (log-entry :entry-type ':processing-reply-pong
                                   :log-level 1
                                   :reply-pong-rec reply-pong-rec
                                   :reply-pong-weight reply-pong-weight
                                   :reply-pong-edges reply-pong-edges
                                   :reply-pong-target reply-pong-target))
                      (setf pong (unify-pongs reply pong :internal-root-set internal-roots)))
          (let ((unified-pong-rec (message-pong-recommendation pong))
                (unified-pong-weight (message-pong-weight pong))
                (unified-pong-edges (message-pong-edges pong))
                (unified-pong-target (message-pong-target-root pong)))
            (log-entry :entry-type ':unified-reply-pong
                       :log-level 1
                       :unified-pong-rec unified-pong-rec
                       :unified-pong-weight unified-pong-weight
                       :unified-pong-edges unified-pong-edges
                       :unified-pong-target unified-pong-target)))))))

(define-process-upkeep ((node blossom-node)) (FINISH-SCAN reply-channel)
  "Finalize the SCAN procedure's stack frames. this includes forwarding the result to a parent if one instigated the SCAN procedure, or spawning a new SUPERVISOR process to handle the result if this SCAN was spontaneous."
  (with-slots (pong) (pop (process-data-stack node))
    (with-slots (recommendation root-bucket) pong
      (cond
        (reply-channel
         (log-entry :entry-type ':pong-throw)
         (if (blossom-node-wilting node)
             (send-message reply-channel (make-pong node))
             (send-message reply-channel pong)))
        ((not (or
               ;; if we're passing, no need to spawn a supervisor
               (eql ':pass recommendation)
               ;; If we're to `HOLD', and we're childless, spawning a supervisor
               ;; will just be a waste of time, due to the fact that there is no
               ;; way anyone else will be held up by us.
               (and (eql ':hold recommendation)
                    (endp (blossom-node-children node)))
               ;; we might have been interrupted and are no longer fit to suggest
               ;; actions for anyone.
               (blossom-node-wilting node)
               (blossom-node-parent node)
               (blossom-node-pistil node)
               (blossom-node-match-edge node)))
         ;; Pause ourselves, as we're about to spawn a supervisor.
         ;; NB: It is the responsibility of the supervisor to unpause us.
         (setf (blossom-node-paused? node) t)

         ;; If we're to `HOLD', set our `HELD-BY-ROOTS' slot to equal the
         ;; root-bucket of the pong, so that the `MULTIREWEIGHT' operation
         ;; can use it.
         (setf (blossom-node-held-by-roots node) root-bucket)
         
         (when (eql 'SCAN-LOOP (first (first (process-command-stack node))))
           (setf (first (process-command-stack node))
                 `(SCAN-LOOP nil)))
         (let ((supervisor (make-supervisor node
                                            :node-class (type-of node)
                                            :node-dryad (blossom-node-dryad node)
                                            :debug? (process-debug? node))))
           (log-entry :entry-type ':spawn-supervisor
                      :pong pong
                      :address (process-public-address supervisor))
           (push pong (process-data-stack supervisor))
           (schedule supervisor (now))))
        (t
         (when (eql 'SCAN-LOOP (first (first (process-command-stack node))))
           (setf (first (process-command-stack node))
                 `(SCAN-LOOP t))))))))

;;;
;;; message handlers
;;;

;;; in the SCAN / PING / ADJOIN-ROOT / PONG family of messages, the expected
;;; message propagation sequence is:
;;;  (1) a tree sends SCAN commands down to all participating vertices.
;;;  (2) each SCAN at a vertex generates PING commands out to all its neighboring
;;;      vertices, including those in foreign trees.
;;;  (3) each PING received is wrapped in an ADJOIN-ROOT, which forwards the PING
;;;      up the receiving tree to its root.
;;;  (4) the root generates a PONG in reply.
;;;  (5) the PONGs are (associatively) aggregated up the originating tree.

;;; NOTE: the current convention is that the SCAN is responsible for accumulating
;;;       the weight adjustments on the source side, then the PING handler adds
;;;       the actual edge weight, and the ADJOIN-ROOT handler accumulates the
;;;       weight adjustments on the recipient side. i think this is the only sane
;;;       arrangement for a source that is ignorant of the recipient's ID.

(define-message-handler handle-message-ping
    ((node blossom-node) (message message-ping))
  "Begins the process of responding to a PING message: starts an ADJOIN-ROOT sequence."
  (with-slots (weight id recipient-child reply-channel root) message
    (let* ((total-weight (+ weight
                            (vertex-vertex-distance (blossom-node-id node) id)))
           (edges (list (make-blossom-edge
                         :target-node nil
                         :target-vertex (process-public-address node))))
           (pong (make-pong node
                            :weight total-weight
                            :edges edges
                            :source-root root
                            :source-id id)))
      (log-entry :entry-type ':handle-ping
                 :ping-type (type-of message)
                 :pingability (blossom-node-pingable node)
                 :vv-distance (vertex-vertex-distance (blossom-node-id node) id)
                 :old-weight weight
                 :new-weight total-weight)
      (send-message (process-public-address node)
                    (funcall (if (typep message 'message-soft-ping)
                                 #'make-message-soft-adjoin-root
                                 #'make-message-adjoin-root)
                             :reply-channel reply-channel
                             :ping message
                             :pong pong)))))

(define-message-handler handle-message-adjoin-root
    ((node blossom-node) (message message-adjoin-root))
  "The workhorse of responding to a PING message: walks up the blossom contractions, then up the maximally-contracted tree, ultimately resulting in a PONG.

This handler is responsible for actually assigning a recommended-next-move for the blossom algorithm, which makes up the bulk of the function body."
  (let* ((ping (message-adjoin-root-ping message))
         (pong (message-adjoin-root-pong message))
         (last-edge (first (message-pong-edges pong))))
    ;; if we haven't yet started crawling up parent instead of pistil...
    (unless (blossom-edge-target-node last-edge)
      ;; ... include these internal weights
      (log-entry :entry-type ':decf-weight
                 :old-value (message-pong-weight pong)
                 :delta (blossom-node-internal-weight node))
      (decf (message-pong-weight pong)
            (blossom-node-internal-weight node)))
    ;; if we haven't yet made it to toplevel...
    (when (blossom-node-pistil node)
      ;; ... keep throwing up pistil.
      (send-message (blossom-node-pistil node) message)
      (finish-handler))
    ;; otherwise, record the first toplevel node we see as our parent blossom.
    ;; CRITICALLY, this does NOT prematurely return.
    (unless (blossom-edge-target-node last-edge)
      (setf (blossom-edge-target-node last-edge)        (process-public-address node)
            (message-adjoin-root-positive? message) (blossom-node-positive? node)
            (message-adjoin-root-match-edge message) (blossom-node-match-edge node)))
    ;; if there's more parents to climb through, do so.
    (when (blossom-node-parent node)
      (send-message (blossom-edge-target-node (blossom-node-parent node))
                    message)
      (finish-handler))
    ;; otherwise, we're at the root.
    (let ((target-root (process-public-address node))
          (recommendation (recommend node ping pong message)))
      (setf (message-pong-target-root pong) target-root
            (message-pong-recommendation pong) recommendation)
      (send-message (message-reply-channel message) pong))))

(defgeneric recommend (node ping pong adjoin-root)
  (:documentation "Computes an action to propose as part of a PONG.")
  (:method ((node blossom-node) ping pong adjoin-root)
    (let ((last-edge (first (message-pong-edges pong)))
          (source-root (message-pong-source-root pong))
          (target-root (process-public-address node))
          (internal-roots (message-ping-internal-roots ping))
          (match-edge (message-adjoin-root-match-edge adjoin-root)))
      (cond
        ((or (and match-edge
                  (address= (slot-value ping 'blossom)
                            (blossom-edge-target-node match-edge)))
             (address= (slot-value ping 'blossom)
                       (blossom-edge-target-node last-edge)))
         ':pass)
        ((not (message-adjoin-root-positive? adjoin-root))
         (push target-root
               (message-pong-root-bucket pong))
         ':hold)
        ((and (not (address= target-root source-root))
              (blossom-node-match-edge node))
         (push (copy-blossom-edge (blossom-node-match-edge node))
               (message-pong-edges pong))
         ':graft)
        ((not (address= target-root source-root))
         ;; When we're MULTIREWEIGHTING, an inter-tree AUGMENT should
         ;; result in a reweighting of half the inter-tree distance.
         ;; We do this here (rather than further down the line) so that
         ;; it can stand on equal footing with other recommendations as
         ;; it is being compared as part of the `unify-pongs' procedure.
         ;; This is somewhat analogous to CONTRACT-BLOSSOM (see below).
         (when (and internal-roots
                    (member source-root internal-roots :test #'address=)
                    (member target-root internal-roots :test #'address=))
           (setf (message-pong-weight pong)
                 (/ (message-pong-weight pong) 2)))
         ':augment)
        ((address= target-root source-root)
         (setf (message-pong-weight pong)
               (/ (message-pong-weight pong) 2))
         ':contract)
        (t
         (error "Unknown blossom case."))))))

(define-message-handler handle-message-scan
    ((node blossom-node) (message message-scan))
  "Begins a scanning process."
  (when (blossom-node-wilting node)
    (when (message-reply-channel message)
      (send-message (message-reply-channel message)
                    (make-pong node)))
    (finish-handler))
  (process-continuation node `(START-SCAN ,message)))
