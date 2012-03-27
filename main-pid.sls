#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library provides the process ID of the main process, which is used to
; separate file names used by instances of the emulator.

(library (vifne main-pid)
  (export
    main-pid-set!
    main-pid-str)
  (import
    (rnrs base)
    (vifne config))

  (define *pid* #F)

  (define (main-pid-set! x)
    (assert (exact-positive-integer? x))
    (set! *pid* x))

  (define (pid) (assert *pid*) *pid*)

  (define (main-pid-str) (number->string (pid)))

)
