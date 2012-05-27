#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library provides for using the exit-handler facility of the Scheme
; system.  This implementation for Ikarus depends on Derick's patched version of
; Ikarus that has an exit-handler facility.

(library (vifne exit)
  (export
    exit-handler)
  (import
    (rnrs base)
    (prefix (only (ikarus) exit-handler) ikarus:))

  (define exit-handler
    ; Save the default exit handler that terminates the process.
    (let ((default (ikarus:exit-handler)))
      (lambda (proc)
        ; Install the wrapped exit handler that terminates.
        (ikarus:exit-handler
         (lambda (x)
           (proc)
           (default x))))))

)
