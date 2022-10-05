;;;; dryad-api.lisp
;;;;
;;;; Specifies the message types used to communicate with the DRYAD.
;;;; See dryad.lisp and matchmaker.lisp for information on the participants.

(in-package #:anatevka)

;;; messages between the dryad and the external world

(defstruct (message-sow (:include message))
  "Instructs a `DRYAD' to inject a new vertex with the indicated id."
  (id nil :type t))

(defstruct (message-reap (:include message))
  "Reported by a `DRYAD' to its `MATCH-ADDRESS' with a pair of IDs that participate in the calculated matching."
  (ids nil :type list))

;;; messages between the dryad and its managed blossoms

(defstruct (message-discover (:include message))
  "Sent from a blossom process to a `DRYAD' to query for a list of other blossom processes to which it should send PINGs."
  (address         nil :type address)
  (id              nil :type t)
  (internal-weight nil :type real) ; NOTE: a little surprised that this isn't (REAL 0)
  (repeat?         nil :type boolean)) 

(defstruct (message-discovery (:include message))
  "The response to a DISCOVER message.

CHANNELS-TO-TRY: The addresses to send PINGs to."
  (channels-to-try nil :type list))

(defstruct (message-wilt (:include message))
  "An instruction to a `BLOSSOM-NODE' to cease operation as a process.")

(defstruct (message-sprout (:include message))
  "Whenever a vertex participates in an augmentation, we are guaranteed that it has been assigned a match (possibly after any parent blossom undergoes expansion).  Upon augmentation, it sends this message to the DRYAD to indicate that it no longer need consider this vertex to be \"live\"."
  (address nil :type address))

;; NOTE: This message is essentially unused in the reference implementation, but it can be useful when implementing an online solver.
(defstruct (message-wilting (:include message))
  "When a `BLOSSOM-NODE' wilts, it notifies its parent `DRYAD' to remove it from consideration."
  (address nil :type address))
