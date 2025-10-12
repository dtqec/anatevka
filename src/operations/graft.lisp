;;;; operations/graft.lisp
;;;;
;;;; If a tree has a weightless edge which targets a matched pair which does not
;;;; participate in any tree, then this matched pair can be subsumed into the
;;;; tree as a child (and petal-child).  For example, if the unmatched node A
;;;; (thought of as a tree with only one node) has a weightless edge to B, and
;;;; B is already matched to C but otherwise is not currently participating in
;;;; the blossom algorithm
;;;;
;;;; A ~~0~~ B === C
;;;;
;;;; then A's tree can be extended as in
;;;;
;;;; A --0-> B ==> C .
;;;;
;;;; It's important to note that a barbell can be grafted onto a bare vertex,
;;;; which would make the bare vertex the root of a new alternating tree, but
;;;; it can also be grafted further down an existing alternating tree, at any
;;;; positive/outer/even node. For example,
;;;;
;;;; A --> B ==> C ~~~ D === E   becomes   A --> B ==> C --> D ==> E .

(in-package #:anatevka)

;;;
;;; supervisor command definitions
;;;

(define-process-upkeep ((supervisor supervisor)) (START-GRAFT pong)
  "Set up the checks for the graft procedure."
  (with-slots (source-root edges) pong
    ;; set up script
    (let* (;; this is the directed edge that corresponds to the barbell
           (neg-pos-edge (first edges))
           (negative-child (blossom-edge-source-node neg-pos-edge))
           (positive-child (blossom-edge-target-node neg-pos-edge))
           ;; this is the recommended edge, that will do the attaching
           (parent-neg-edge (second edges))
           ;; the `source-root' is the root of the tree that recommended
           ;; this action, which may or may not be equal to `graft-parent'
           (targets (remove-duplicates
                     (list source-root negative-child positive-child)
                     :test #'address=)))
      (process-continuation supervisor `(HALT))
      ;; check that there are three nodes and that they form a chain
      (unless (and (= 3 (length targets))
                   (address= negative-child
                             (blossom-edge-target-node parent-neg-edge)))
        (setf (process-lockable-aborting? supervisor) t)
        (finish-handler))
      (process-continuation supervisor
                            `(BROADCAST-LOCK ,targets)
                            `(CHECK-ROOTS (,source-root))
                            `(BROADCAST-PINGABILITY ,targets :SOFT)
                            `(CHECK-PONG ,pong)
                            `(INNER-GRAFT ,pong)
                            `(CLEAR-HELD-BY-ROOTS ,targets)
                            `(BROADCAST-UNLOCK)))))

(define-process-upkeep ((supervisor supervisor)) (INNER-GRAFT pong)
  "Actually perform the graft procedure."
  (unless (process-lockable-aborting? supervisor)
    (with-slots (source-root edges) pong
      (let* (;; this is the directed edge that corresponds to the barbell
             (neg-pos-edge (first edges))
             (pos-neg-edge (reverse-blossom-edge neg-pos-edge))
             (negative-child (blossom-edge-source-node neg-pos-edge))
             (positive-child (blossom-edge-target-node neg-pos-edge))
             ;; this is the recommended edge, that will do the attaching
             (parent-neg-edge (second edges))
             (neg-parent-edge (reverse-blossom-edge parent-neg-edge))
             ;; this is the node that we want to attach our barbell to
             (graft-parent (blossom-edge-source-node parent-neg-edge)))
        (process-continuation supervisor
                              ;; the `negative-child's parent is the `graft-parent'
                              `(INSTALL-PARENT ,negative-child ,neg-parent-edge)
                              ;; the `positive-child's parent is the `negative-child'
                              `(INSTALL-PARENT ,positive-child ,pos-neg-edge)
                              ;; the `graft-parent's child is the `negative-child'
                              `(INSTALL-CHILD ,graft-parent ,parent-neg-edge)
                              ;; the `negative-child's child is the `positive-child'
                              `(INSTALL-CHILD ,negative-child ,neg-pos-edge)
                              `(INSTALL-POSITIVITY ,negative-child ,nil)
                              `(INSTALL-POSITIVITY ,positive-child ,t))))))

(define-process-upkeep ((supervisor supervisor))
    (INSTALL-PARENT target edge)
  "Sets a target's parent."
  (unless (process-lockable-aborting? supervisor)
    (sync-rpc (make-message-set :slots '(parent) :values `(,edge))
        (set-result target)
      nil)))

(define-process-upkeep ((supervisor supervisor))
    (INSTALL-CHILD target edge)
  "Appends a child to a target."
  (unless (process-lockable-aborting? supervisor)
    (sync-rpc (make-message-push :slot 'children :value edge)
        (push-result target)
      nil)))

(define-process-upkeep ((supervisor supervisor))
    (INSTALL-POSITIVITY target positive?)
  "Sets a target's POSITIVE? field."
  (unless (process-lockable-aborting? supervisor)
    (sync-rpc (make-message-set :slots '(positive?) :values `(,positive?))
        (set-result target)
      nil)))
