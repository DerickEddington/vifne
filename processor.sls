#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library implements an emulated processor.  It must only be used after
; forking a process for a processor, after the storage is properly initialized.

; TODO: How to handle user-caused processor exceptions?

(library (vifne processor)
  (export
    start-processor)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs lists)
    (rnrs exceptions)
    (rnrs conditions)
    (vifne config)
    (vifne posix)
    (only (vifne storage) f fv fp?)
    (vifne message-queue)
    (vifne processor array)
    (vifne processor registers)
    (vifne processor operations)
    (vifne processor exception))

  (define sid)
  (define storage)
  (define replies)

  (define (send* x) (send storage (cons* (car x) sid (cdr x))))
  (define (receive*) (receive replies))


  (define (start-processor n sth rth rtt)
    (define name (string-append "processor" (number->string n)))

    (define (main)
      ; Create this processor's message queue for replies from the storage
      ; controller.
      (set! replies (create-message-queue name))
      ; If the storage controller process has not created its message queue yet,
      ; wait a second and try again up to 5 times.
      (let retry ((max 5))
        (unless (positive? max)
          (error 'start-processor "storage-controller queue does not exist"))
        (guard (ex ((error? ex)
                    (sleep 1)
                    (retry (- max 1))))
          (set! storage (open-message-queue "storage-controller"))))
      ; Register with the storage controller.
      (send storage `(processor ,name))
      ; Get the ID the storage controller assigned to this processor.
      (set! sid (cadr (receive*)))

      ; Initialize the sub-libraries.  This is done like this to avoid import
      ; circles.
      (registers:initialize! send*)
      (operations:initialize! send* receive* rth rtt
        ; The first process to do this will create this queue, the others will
        ; just open it.
        (create-or-open-message-queue "waiting-processors"))

      #|; Save the chunk IDs of the startup-tasks stream, for when the processor
      ; stops.  TODO: Why? What was my thinking?  For automatic freezing of
      ; active tasks?
      (set! startup-tasks-head-id sth)
      (set! startup-tasks-tail-id stt)|#

      ; Start executing instructions.
      (send* `(stream-get ,sth #F #F))
      (let ((x (receive*)))
        (if (list? x)
          (let ((x (cadr x)))
            (assert (fp? x))
            ; Prepare to transfer control to the task.
            (activate (fv x))
            ; Lose reference to task chunk. (What it referenced is now
            ; referenced by registers).
            (send* `(decrement ,(fv x))))
          (begin
            (assert (eq? 'stream-empty x))
            ; Wait for a task to become available in the global RTQ.
            (activate-ready-task))))
      ; Execute instructions.
      (instruction-interpreter))

    (define (before-death)
      (clear-registers!)
      (send* '(terminated)))

    (define (after-death)
      (destroy-message-queue name)
      ; This will raise an exception for all but one processor process.  The
      ; parent process will abort an after-death because of an exception, so
      ; this must be the last command of this procedure.  The exception is not
      ; suppressed here, so a log of it being ignored is recorded, in case it's
      ; not the expected exception.
      (destroy-message-queue "waiting-processors"))

    (values name main before-death after-death))


  (define (instruction-interpreter)

    (define (get-inst id i)
      (let ((inst-word (array-ref id i)))
        (when (fp? inst-word) (processor-exception 'invalid-instruction))
        (fv inst-word)))

    ; Check if this process has been told to terminate.  Done between
    ; instructions to ensure they are not interrupted (only SIGKILL or computer
    ; failure can interrupt, hopefully).
    (unless (memq 'SIGTERM (sigpending))
      ; Execute an instruction.
      (assert (srp? IS))
      (assert (not (srp? II)))
      (let ((id (srv IS)) (i (srv II)))
        (sr-set! II (f (+ 1 i) #F))
        (guard (ex #;((processor-exception? ex) TODO))
          (do-operation (get-inst id i))))
      (instruction-interpreter)))

  ;-----------------------------------------------------------------------------

  ; This library, and its sub-libraries, assume a word size of 64 bits and 16
  ; words per chunk.

  (assert (= 8 word-size))
  (assert (= 16 chunk-wsz))

)
