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
    (vifne util assembler storage)
    (vifne storage)
    (vifne storage stream)
    (vifne config))

  (define (add-startup-task! chunk-id adjustment)

    (define (adjust! count proc!)
      (when (positive? count)
        (proc! chunk-id)
        (adjust! (- count 1) proc!)))

    (define (task is)
      (new-chunk (list (new-magic-pointer is) 0 ; entry point
                       0                        ; Return special register value
                       0 0                      ; arg0
                       0 0)))                   ; arg1

    (assert (valid-id? chunk-id))
    (assert (exact-integer? adjustment))

    (let* ((task-id (store! (task chunk-id) #T))
           ; Tell stream-put! to not increment the reference count of the task
           ; chunk, because store! already set it to 1.
           (x (stream-put! (startup-tasks-tail) (f task-id #T) #F #F)))
      (assert (not x)))

    (apply adjust! (if (negative? adjustment)
                     (list (- adjustment) decr-refcount!)
                     (list adjustment incr-refcount!))))

)
