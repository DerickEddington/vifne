#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library is the emulated processor.  This library must be used only after
; forking a process for a processor.

(library (vifne processor)
  (export
    start-processor)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs exceptions)
    (rnrs conditions)
    (vifne posix)
    (vifne message-queue))

  (define sid)
  (define storage)
  (define replies)

  (define (start-processor n)
    (define rmqn (string-append "processor" (number->string n)))
    (set! replies (create-message-queue rmqn))
    ; If the storage controller process has not created its queue yet, wait a
    ; second and try again.
    (let retry ((max 5))
      (unless (positive? max)
        (error 'start-processor "storage-controller queue does not exist"))
      (guard (ex ((error? ex)
                  (sleep 1)
                  (retry (- max 1))))
        (set! storage (open-message-queue "storage-controller"))))
    ; Register with the storage controller.
    (send storage `(processor ,rmqn))
    ; Get the ID the storage controller assigned to this processor.
    (set! sid (cadr (receive replies)))
    ; TODO: Start executing emulated stuff...
    )

)
