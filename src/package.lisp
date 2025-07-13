;;;; package.lisp

#-(or sbcl ecl ccl)
(rename-package :alexandria :alexandria '(:a))

(defpackage #:anatevka
  (:use #:cl)
  (:use #:aether)
  
  #+(or sbcl ecl ccl)
  (:local-nicknames (:a :alexandria))

  ;; symbols for export
  
  ;; matchmaker.lisp
  (:export
   #:blossom-node                       ; CLASS
   #:make-blossom-edge                  ; FUNCTION
   )
  
  ;; utilities.lisp
  (:export
   #:with-with                          ; MACRO
   )

  ;; node.lisp
  (:export
   #:min-id                             ; GENERIC
   )

  ;; dryad.lisp / dryad-api.lisp
  (:export
   #:dryad                              ; CLASS
   #:*dryad-default-clock-rate*         ; PARAMETER
   #:dryad-match-address                ; ACCESSOR
   #:dryad-ids                          ; ACCESSOR
   #:dryad-sprouted?                    ; ACCESSOR
   #:dryad-shuffle?                     ; ACCESSOR
   #:vertex-vertex-distance             ; GENERIC FUNCTION
   
   ;; messages
   #:message-sow                        ; TYPE
   #:message-reap                       ; TYPE
   #:message-discover                   ; TYPE
   #:message-discovery                  ; TYPE
   #:message-wilt                       ; TYPE
   #:message-sprout                     ; TYPE
   #:message-wilting                    ; TYPE
   
   #:make-message-sow                   ; FUNCTION
   #:make-message-reap                  ; FUNCTION
   #:make-message-discover              ; FUNCTION
   #:make-message-discovery             ; FUNCTION
   #:make-message-wilt                  ; FUNCTION
   #:make-message-sprout                ; FUNCTION
   #:make-message-wilting               ; FUNCTION
   
   #:message-sow-id                     ; ACCESSOR
   #:message-sow-event?                 ; ACCESSOR
   #:message-reap-ids                   ; ACCESSOR
   #:message-discover-address           ; ACCESSOR
   #:message-discover-id                ; ACCESSOR
   #:message-discover-internal-weight   ; ACCESSOR
   #:message-discover-strategy          ; ACCESSOR
   #:message-discovery-channels-to-try  ; ACCESSOR
   #:message-discovery-future-distance  ; ACCESSOR
   #:message-sprout-address             ; ACCESSOR
   #:message-wilting-address            ; ACCESSOR
   )

  ;; logger.lisp
  (:export
   #:supervisor-logs-for-address        ; FUNCTION
   #:successful-supervisors             ; FUNCTION
   #:remove-unsuccessful-supervisors    ; FUNCTION
   #:print-reduced-log                  ; FUNCTION
   )
  )
