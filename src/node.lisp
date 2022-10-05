;;;; matchmaker.lisp
;;;;
;;;; This implements the fundamental computational actor in a distributed
;;;; modification of Edmonds's minimum-weight perfect matching algorithm.

(in-package #:anatevka)

;;; some useful enumeratives

(deftype ping-type ()
  '(member :ALL :SOFT :NONE))

;;;
;;; BLOSSOM-NODE definition.
;;;
;;; BLOSSOM-NODEs participate in two different trees:
;;;   (1) There is a _forest_ structure built up from alternating chains of
;;;       unmatched and matched edges, rooted at an unmatched vertex.  This tree
;;;       structure is stored in the PARENT and CHILDREN slots.
;;;   (2) A node can also represent a (possibly iteratively) contracted
;;;       subgraph. These contractions are injected so as to move the overall
;;;       graph structure towards being bipartite.  Such a contracted subgraph
;;;       is commonly referred to as a "blossom", though we also use the word to
;;;       refer to bare vertices.  This tree structure (where a BLOSSOM carries
;;;       pointers to the vertices of which it's a contraction, with the
;;;       guarantee that no vertices are ever shared among different blossoms in
;;;       this way) is stored in the PISTIL and PETALS slots.
;;;
;;; It also tracks a small amount of miscellaneous blossom algorithm state info.
;;;

(defparameter *blossom-node-clock-rate* 50
  "Determines the number of actions a blossom process takes per unit of simulation time. This is used to initialize (PROCESS-CLOCK-RATE BLOSSOM), offset so that it catches the eye.")

(defclass blossom-node (process-lockable)
  (;; process-related slots, including overrides
   (process-clock-rate
    :initform *blossom-node-clock-rate*
    :documentation "Number of blossom actions per unit of simulation time. Overrides the parent slot provided by PROCESS.")
   ;; the dryad is our parent process. it also keeps track of "neighboring"
   ;; vertices (i.e., the graph structure of the problem we're trying to solve),
   ;; we retain its address so that we can ask it for a list of active neighbors.
   (dryad
    :accessor blossom-node-dryad
    :initform nil
    :initarg :dryad
    :type (or null address)
    :documentation "The address of the host DRYAD.")
   ;; most of the process's state is maintained in its data stack, but there are
   ;; also globals that are useful to track separately, as they govern how the
   ;; process responds to (or mutes) incoming messages.
   (pingable
    :accessor blossom-node-pingable
    :initform ':ALL
    :type ping-type
    :documentation "Used by message handler guards to determine whether this process is currently servicing PING requests. SOFT-PINGs are serviced when set to :SOFT or :ALL, PINGs are serviced when set to :ALL, nothing is serviced when set to :NONE.")
   (wilting
    :accessor blossom-node-wilting
    :initform nil
    :initarg :wilting
    :type boolean
    :documentation "If T, BLOSSOM-NODE has lost the will to live.")
   (paused?
    :accessor blossom-node-paused?
    :initform nil
    :initarg :paused?
    :type boolean
    :documentation "If T, do not start a new scan.")
   ;; the remaining slots encode the "data" of the blossom algorithm's state:
   ;; its tree structures, any reweightings, and so on. (it does not capture
   ;; all of the "execution" state of the algorithm: that is captured in frames
   ;; on the process's data stack.)
   (id
    :accessor blossom-node-id
    :initarg :id
    :documentation "Internal name for this blossom node. Used by VERTEX-VERTEX-DISTANCE to the edge weight in the graph.")
   (match-edge
    :accessor blossom-node-match-edge
    :initarg :match-edge
    :initform nil
    :type (or null blossom-edge)
    :documentation "If this vertex is matched, point to the match.")
   (internal-weight
    :accessor blossom-node-internal-weight
    :initarg :internal-weight
    :initform 0
    :type real
    :documentation "The dual weight y_v in the algorithm.")
   (pistil
    :accessor blossom-node-pistil
    :initform nil
    :initarg :pistil
    :type (or null address)
    :documentation "(Immediate) blossom that this vertex belongs to.")
   (petals
    :accessor blossom-node-petals
    :initform nil
    :initarg :petals
    :type list
    :documentation "List of BLOSSOM-EDGEs. Runs through nodes in this blossom in cyclic order.")
   (parent
    :accessor blossom-node-parent
    :initform nil
    :initarg :parent
    :type (or null blossom-edge)
    :documentation "If this blossom is part of a nontrivial tree, point to parent.")
   (children
    :accessor blossom-node-children
    :initform nil
    :initarg :children
    :type list
    :documentation "If this blossom is part of a tree, these are its children.")
   (positive?
    :accessor blossom-node-positive?
    :initform t
    :initarg :positive?
    :type boolean
    :documentation "Parity of distance from root, as in C_2.")
   (held-by-roots
    :accessor blossom-node-held-by-roots
    :initform nil
    :initarg :held-by-roots
    :type list
    :documentation "LIST of `BLOSSOM-NODE' roots who are causing us to `HOLD'."))
  (:documentation "Embodies a blossom in the blossom algorithm."))

;;;
;;; basic utilities for BLOSSOM-NODE instances
;;;

(defmethod print-object ((object blossom-node) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream ":ID ~a :ADDRESS ~a"
            (blossom-node-id object)
            (process-public-address object))))

(defun vertex? (node)
  "Is NODE a VERTEX? (That is: does it have blossom children that it's contracting?)"
  (check-type node blossom-node)
  (not (blossom-node-petals node)))

(defgeneric vertex-vertex-distance (id-v id-w)
  (:documentation "Returns the distance between the blossom vertex with ID given by ID-V and that with ID given by ID-W."))

;;;
;;; the BLOSSOM-EDGE structure, which describes a (directed) edge from one
;;; (possibly contracted) vertex to another, and associated utilities.
;;;

(defstruct (blossom-edge (:constructor %make-blossom-edge))
  "Represents a directed edge between two blossoms X and Y, as connected by vertices A, B.

[X: ... A--]--[->B ... :Y]"
  (source-node   nil :type (or null address))
  (source-vertex nil :type (or null address))
  (target-node   nil :type (or null address))
  (target-vertex nil :type (or null address)))

(defun make-blossom-edge (&key (source-node nil source-node-p)
                               source-vertex
                               target-vertex
                               (target-node nil target-node-p))
  (%make-blossom-edge :source-node (if source-node-p source-node source-vertex)
                      :source-vertex source-vertex
                      :target-vertex target-vertex
                      :target-node (if target-node-p target-node target-vertex)))

(defmethod print-object ((object blossom-edge) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (if (and (blossom-edge-source-node object)
             (blossom-edge-source-vertex object)
             (address= (blossom-edge-source-node object)
                       (blossom-edge-source-vertex object)))
        (format stream "~a-"
                (blossom-edge-source-vertex object))
        (format stream "[~a: ~a--]"
                (blossom-edge-source-node object)
                (blossom-edge-source-vertex object)))
    (format stream "--")
    (if (and (blossom-edge-target-vertex object)
             (blossom-edge-target-node object)
             (address= (blossom-edge-target-vertex object)
                       (blossom-edge-target-node object)))
        (format stream "->~a"
                (blossom-edge-target-vertex object))
        (format stream "[->~a :~a]"
                (blossom-edge-target-vertex object)
                (blossom-edge-target-node object)))))

(defun edge= (x y)
  (check-type x blossom-edge)
  (check-type y blossom-edge)
  (and (address= (blossom-edge-source-node   x) (blossom-edge-source-node   y))
       (address= (blossom-edge-source-vertex x) (blossom-edge-source-vertex y))
       (address= (blossom-edge-target-vertex x) (blossom-edge-target-vertex y))
       (address= (blossom-edge-target-node   x) (blossom-edge-target-node   y))))

(defun reverse-blossom-edge (edge)
  "(Nondestructively) reverses the directionality of EDGE."
  (make-blossom-edge :source-node (blossom-edge-target-node edge)
                     :source-vertex (blossom-edge-target-vertex edge)
                     :target-vertex (blossom-edge-source-vertex edge)
                     :target-node (blossom-edge-source-node edge)))

(defun reverse-blossom-edges (edges &optional acc)
  "(Nondestructively) reverses the directionality of a path of EDGEs."
  (unless edges
    (return-from reverse-blossom-edges acc))
  (let ((first (first edges))
        (rest (rest edges)))
    (push (reverse-blossom-edge first) acc)
    (reverse-blossom-edges rest acc)))

(defun find-even-arm (cycle start end &key
                                        (key #'identity)
                                        (test #'eql)
                                        (rev #'reverse)
                                        reversed?)
  "Given an odd-length list of items, stored as `CYCLE', in which `START' and `END' appear, finds the even-length path connecting `START' and (just before) `END'. To find this path, we make use of the `KEY' and `TEST' keyword arguments as inputs to the `POSITION' function. Returns a VALUES triple: the even-length subpath, a boolean indicating whether the path proceeds in an opposite direction from that natural orientation of `CYCLE' (i.e., if it proceeds from END to START), and the full cycle rotated to begin just after `END' and potentially reversed to match the orientation of the subpath. If reversal is necessary, we use the function specified by the `REV' keyword argument to reverse the `CYCLE' and recurse to find the reverse paths.

For instance,

   (find-even-arm (list 1 2 3 4 5 6 7) 5 3)

evalutes to

   (5 4)
   T
   (3 2 1 7 6 5 4) ."
  (let* ((length (length cycle))
         (doubled (append cycle cycle))
         (left (position start doubled :key key :test test))
         (right (position end doubled :key key :test test :start left)))
    (cond
      ((evenp (- right left))
       (values (subseq doubled left right)
               reversed?
               (nconc (subseq doubled right (+ left length))
                      (subseq doubled left right))))
      (t
       (assert (null reversed?) () "cha cha real slow")
       (find-even-arm (funcall rev cycle)
                      start end
                      :key key :test test :rev rev :reversed? T)))))

;;;
;;; message definitions for BLOSSOM-NODE
;;;

(defstruct (message-broadcast-pingability (:include message))
  "Sent from a `SUPERVISOR' to a tree to change its pingability to `PING-TYPE'."
  (ping-type nil :type ping-type))

(defstruct (message-set (:include message))
  "Causes a remote SETF (on the BLOSSOM-NODE object).  The nth slot is set to the nth value."
  (slots  nil :type list)
  (values nil :type list))

(defstruct (message-push (:include message))
  "Causes a remote PUSH (on the BLOSSOM-NODE object). The VALUE is pushed to the head of SLOT."
  (slot  nil :type symbol)
  (value nil :type t))

(defstruct (message-values (:include message))
  "Replies with slot-values (on the BLOSSOM-NODE object)."
  (values nil :type list))

(defstruct (message-id-query (:include message))
  "Replies with the minimum ID at this macrovertex."
  )

;;;
;;; message handlers for BLOSSOM-NODE
;;;

(defgeneric min-id (x y)
  (:documentation "Computes the smaller of two IDs.")
  (:method ((x real) (y real))
    (min x y))
  (:method ((x string) (y string))
    (if (string< x y) x y)))

(define-message-subordinate handle-message-id-query
    ((node blossom-node) (message message-id-query) now)
  "Replies with the minimum ID at this macrovertex."
  (cond
    ((null (blossom-node-petals node))
     (send-message (message-reply-channel message)
                   (make-message-rpc-done :result (blossom-node-id node))))
    (t
     ;; NOTE: this could be a broadcast call if aether subordinates subclassed that
     (with-replies (replies)
                   (send-message-batch #'make-message-id-query
                                       (mapcar #'blossom-edge-target-node
                                               (blossom-node-petals node)))
       (send-message (message-reply-channel message)
                     (make-message-rpc-done
                      :result (reduce #'min-id (rest replies))))))))

;; When locked, a tree delays any replies to PING messages, lest a PONG reply be
;; calculated while the tree is in a dirty state. This on its own is too
;; limiting: a tree must also be able to send and receive PINGs in order to
;; check that its proposed operation is still sane / has produced sane results.
;; We enable this by changing the tree's pingability, and thus permitting the
;; tree to respond to a safe subset (or to all) of PING requests.

(define-broadcast-handler handle-message-broadcast-pingability
    ((node blossom-node) (message message-broadcast-pingability) now)
  "Changes the pingability of `NODE' (and children / petals) to `PING-TYPE'."
  (with-slots (ping-type) message
    (log-entry :entry-type 'changing-pingability
               :old-pingability (blossom-node-pingable node)
               :new-pingability ping-type)
    (setf (blossom-node-pingable node) ping-type)
    (push-broadcast-frame :targets (mapcar #'blossom-edge-target-node
                                           (union (blossom-node-petals node)
                                                  (blossom-node-children node))))))

;; rather than separately implement a plethora of micromessages that serve as
;; accessors, we provide a handful of messages which serve as generic setters
;; and getters on the receiver.
;;
;; NOTE GH-140: these are probably pretty easy to abuse. perhaps it would be
;;     better to implement the micromessages after all.

(define-rpc-handler handle-message-set
    ((node blossom-node) (message message-set) now)
  "Handles a remote SETF request."
  (with-slots (slots values reply-channel) message
    (loop :for slot :in slots
          :for value :in values
          :do (setf (slot-value node slot) value))
    (values)))

(define-rpc-handler handle-message-push
    ((node blossom-node) (message message-push) now)
  "Handles a remote PUSH request."
  (with-slots (slot value reply-channel) message
    (push value (slot-value node slot))
    (values)))

(define-rpc-handler handle-message-values
    ((node blossom-node) (message message-values) now)
  "Handles a remote request for data."
  (with-slots (values reply-channel) message
    (loop :for value :in values
          :collect (slot-value node value))))

;; the SPROUT-ON-BLOSSOM and WILT messages pertain to the blossom lifecycle with
;; regards to the owning dryad. after originally initializing the blossom, the
;; dryad is interested in when the blossom begins participating in matches
;; (before which it's certainly unable to be considered "done") and, conversely,
;; in the ability to inform a blossom that it's been removed from participating
;; and should halt its process.

(define-message-handler handle-message-sprout-on-blossom
    ((node blossom-node) (message message-sprout) now)
  "Handles a request that a root node (perhaps not a vertex) alert the DRYAD that it has sprouted."
  (cond
    ((blossom-node-petals node)
     (let* ((first-child (first (blossom-node-petals node)))
            (peduncle-node (blossom-edge-source-node first-child)))
       (send-message peduncle-node message)))
    (t
     (send-message (blossom-node-dryad node)
                   (make-message-sprout :address (process-public-address node))))))

(define-message-handler handle-message-wilt
    ((node blossom-node) (message message-wilt) now)
  ;; sanity check: are we actually allowed to wilt?
  (when (or (blossom-node-parent node)
            (blossom-node-pistil node)
            (blossom-node-children node)
            (blossom-node-petals node)
            (null (blossom-node-match-edge node)))
    (error "Caught wilt message, but not decoupled from tree."))
  (send-message (blossom-node-dryad node)
                (make-message-wilting :reply-channel (message-reply-channel message)
                                      :address (process-public-address node)))
  (setf (blossom-node-wilting node) t))

;;;
;;; blossom message dispatch table
;;;

;; NOTE: the ordering of this table _mostly_ doesn't matter.  its only really
;;       important feature is that LOCK-REQUEST gets handled with high priority.
(define-message-dispatch blossom-node
  (message-soft-adjoin-root           'handle-message-adjoin-root
                                      (typep (blossom-node-pistil blossom-node)
                                             '(or null address)))
  (message-adjoin-root                'handle-message-adjoin-root
                                      (and (eql ':ALL (blossom-node-pingable blossom-node))
                                           (typep (blossom-node-pistil blossom-node)
                                                  '(or null address))))
  
  (message-lock                       'handle-message-lock)
  
  (message-broadcast-reweight         'handle-message-broadcast-reweight
                                      (process-lockable-locked? blossom-node))
  
  (message-percolate                  'handle-message-percolate)
  
  (message-soft-scan                  'handle-message-scan
                                      (not (eql ':NONE (blossom-node-pingable blossom-node))))
  (message-scan                       'handle-message-scan
                                      (eql ':ALL (blossom-node-pingable blossom-node)))

  (message-broadcast-pingability      'handle-message-broadcast-pingability)

  (message-convergecast-collect-roots 'handle-message-convergecast-collect-roots)
  
  (message-set                        'handle-message-set)
  (message-push                       'handle-message-push)
  (message-values                     'handle-message-values)
  
  (message-root-path                  'handle-message-root-path)
  (message-attach-parent              'handle-message-attach-parent)
  (message-convert-child-to-petal     'handle-message-convert-child-to-petal)
  (message-reattach-cycle-child       'handle-message-reattach-cycle-child)
  (message-set-up-blossom             'handle-message-set-up-blossom)
  
  (message-expand                     'handle-message-expand)
  (message-blossom-parent             'handle-message-blossom-parent
                                          (typep (blossom-node-pistil blossom-node)
                                                 '(or null address)))
  (message-replace-child              'handle-message-replace-child)
  
  (message-soft-ping                  'handle-message-ping
                                          (not (eql ':NONE (blossom-node-pingable blossom-node))))
  
  (message-ping                       'handle-message-ping
                                          (eql ':ALL (blossom-node-pingable blossom-node)))
  
  (message-wilt                       'handle-message-wilt)
  
  (message-sprout                     'handle-message-sprout-on-blossom)
  
  (message-id-query                   'handle-message-id-query))

;;;
;;; basic command definitions for BLOSSOM-NODE
;;;

(define-process-upkeep ((node blossom-node) now) (START)
  "Blossom nodes represent (contracted subgraphs of) vertex(es).  The START command drops the blossom node into an infinite loop, SCAN-LOOP, which enacts the basic behavior."
  (process-continuation node `(SCAN-LOOP)))

(define-process-upkeep ((node blossom-node) now) (SCAN-LOOP &optional repeat?)
  "If we're out of things to do & unmatched, consider starting a SCAN.  If REPEAT? is set, then this is  _not_ our first time trying to SCAN to find something to do, and the previous attempt(s) resulted in no action."
  (unless (blossom-node-wilting node)
    (process-continuation node `(SCAN-LOOP))
    (unless (or (process-lockable-locked? node)
                (blossom-node-parent node)
                (blossom-node-pistil node)
                (blossom-node-match-edge node)
                (blossom-node-paused? node))
      ;; doing this manual command injection rather than sending a message is a
      ;; stopgap against sending multiple SCAN messages, which looks gross / wrong.
      (let ((scan-message (make-message-scan
                           :local-root (process-public-address node)
                           :weight 0
                           :repeat? repeat?)))
        
        (process-continuation node `(START-SCAN ,scan-message))))))

(define-process-upkeep ((node blossom-node) now) (IDLE)
  (unless (blossom-node-wilting node)
    (process-continuation node `(IDLE))))
