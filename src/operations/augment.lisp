;;;; operations/augment.lisp
;;;;
;;;; An AUGMENT action enlarges a matching by introducing two new nodes to it.
;;;; Recall that the blossom forest is populated by trees which are built out of
;;;; and unmatched root node and that all other nodes in the tree are matched to
;;;; other nodes in the tree (cf. GRAFT).  So, a prototypical pair of trees
;;;; might look like this:
;;;;
;;;; A --> B ==> C
;;;;
;;;;    E ==> F
;;;;   ^
;;;;  /
;;;; D
;;;;  \
;;;;   v
;;;;    G ==> H
;;;;
;;;; where A, ..., H are nodes, --> and ==> indicate a parent/child relationship,
;;;; and == indicates a match between its endpoints.  An AUGMENT is triggered by
;;;; a weightless edge C ~~ F.  Tracing the paths from C and from F to their
;;;; respective roots, we have
;;;;
;;;; D --> E ==> F ~~ C <== B <-- A .
;;;;
;;;; AUGMENTATION "reverses" which edges in this chain are matches, as in
;;;;
;;;; D === E     F == C     B === A,
;;;;
;;;; and it destroys all of the parent/child relationships within the tree, so
;;;; that any other nonparticipating branches in the tree are reduced to their
;;;; constituent barbells:
;;;;
;;;; G == H.
;;;;
;;;; Critically, AUGMENT is only considered for edges between nodes which are
;;;; both "positive" (i.e., of even height) in their respective trees, so that
;;;; the path to their respective roots has the indicated alternating pattern.

(in-package #:anatevka)

;;;
;;; supervisor command definitions
;;;

(define-process-upkeep ((supervisor supervisor)) (START-AUGMENT pong)
  "Sets up the augmentation procedure."
  (with-slots (edges source-root target-root) pong
    (let* ((edge (first edges))
           (targets (list source-root target-root)))
      ;; set up script
      (process-continuation supervisor
                            `(BROADCAST-LOCK ,targets)
                            `(CHECK-ROOTS ,targets)
                            `(BROADCAST-PINGABILITY ,targets :SOFT)
                            `(CHECK-PONG ,pong)
                            `(AUGMENT ,edge)
                            `(AUGMENT ,(reverse-blossom-edge edge))
                            `(BROADCAST-UNLOCK :destroy? ,T)
                            `(HALT)))))

(define-process-upkeep ((supervisor supervisor)) (AUGMENT edge)
  "Perform an augmentation along a given edge."
  (unless (process-lockable-aborting? supervisor)
    (log-entry :entry-type 'augment
               :from (blossom-edge-source-node edge)
               :to (blossom-edge-target-node edge))
    (sync-rpc (make-message-percolate :traversal-edge edge)
        (percolate-result (blossom-edge-target-node edge))
      nil)))

;;;
;;; message definitions
;;;

(defstruct (message-percolate (:include message))
  "Sent from a SUPERVISOR to a tree (and then internally to a tree) to cause a path augmentation."
  (traversal-edge nil :type blossom-edge))

;;;
;;; message handlers
;;;

(define-message-handler handle-message-percolate
    ((node blossom-node) (message message-percolate))
  "Performs a step in the path augmentation process."
  (with-slots (traversal-edge reply-channel) message
    ;; does the previous node expect me to link to it?
    (let ((back-linking?
            (or (null (blossom-node-parent node))
                (and (blossom-node-match-edge node)
                     (address=
                      (blossom-edge-target-node (blossom-node-parent node))
                      (blossom-edge-target-node (blossom-node-match-edge node)))))))
      (cond
        ;; if we're the top node...
        ((null (blossom-node-parent node))
         ;; tie us off
         (setf (blossom-node-match-edge node) (reverse-blossom-edge traversal-edge))
         ;; sprout this root node
         (send-message (process-public-address node)
                       (make-message-sprout :address (process-public-address node)))
         ;; and announce the completion
         (send-message reply-channel (make-message-rpc-done)))
        ;; otherwise, we have a parent and we propagate upward
        (t
         (setf (blossom-node-match-edge node)
               (if back-linking?
                   (reverse-blossom-edge traversal-edge)
                   (copy-blossom-edge (blossom-node-parent node))))
         (send-message (blossom-edge-target-node (blossom-node-parent node))
                       (make-message-percolate
                        :reply-channel reply-channel
                        :traversal-edge (copy-blossom-edge (blossom-node-parent node)))))))))
