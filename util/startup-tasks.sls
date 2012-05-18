#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library provides means for externally manually configuring the
; startup-tasks stream of storage files.  This is not how a startup-tasks stream
; normally will be configured - normally some operating-system software running
; in the emulator will do it.  This library must only be used after the storage
; file and the storage library are properly initialized.

(library (vifne util startup-tasks)
  (export
    add-startup-task!)
  (import
    (rnrs base)
    (rnrs control)
    (vifne storage)
    (vifne storage stream)
    (vifne config))

  (define (add-startup-task! chunk-id adjustment)

    (define (adjust! count proc!)
      (when (positive? count)
        (proc! chunk-id)
        (adjust! (- count 1) proc!)))

    (assert (valid-id? chunk-id))
    (assert (exact-integer? adjustment))

    (stream-put! (startup-tasks-tail) chunk-id #T)

    (apply adjust! (if (negative? adjustment)
                     (list (- adjustment) decr-refcount!)
                     (list adjustment incr-refcount!))))

)
