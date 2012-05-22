#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library provides for using the exit-handlers facility of the Scheme
; system.  This implementation for Ikarus depends on Derick's patched version of
; Ikarus that has an exit-handlers facility.

(library (vifne exit)
  (export
    (rename (push-exit-handler! register-exit-handler!))
    remove-exit-handlers!)
  (import
    (rnrs base)
    (rnrs control)
    (only (ikarus) push-exit-handler! pop-exit-handler!))

  (define (remove-exit-handlers!)
    (when (pop-exit-handler!)
      (remove-exit-handlers!)))

)
