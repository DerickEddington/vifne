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
    (vifne message-queue)
    (vifne processor array)
    (vifne processor registers)
    (vifne processor operations)
    (vifne processor exception))

  (define sid)
  (define storage)
  (define replies)
  (define startup-tasks-head-id)
  (define startup-tasks-tail-id)

  (define (send* x) (send storage (cons* (car x) sid (cdr x))))
  (define (receive*) (receive replies))


  (define (start-processor n sth stt)
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

      ; Initialize the sub-libraries that must be able to message with the
      ; storage controller.  This is done like this to avoid import circles.
      (for-each (lambda (proc) (proc send* receive*))
                (list registers:set-storage-comm!
                      operations:set-storage-comm!))

      ; Save the chunk IDs of the startup-tasks stream, for when the processor
      ; stops.
      (set! startup-tasks-head-id sth)
      (set! startup-tasks-tail-id stt)

      ; Start executing instructions.
      (send* `(stream-get ,startup-tasks-head-id #F))
      (let ((x (receive*)))
        (if (list? x)
          (let ((x (cadr x)))
            (assert (fp? x))
            ; Set the Instruction Segment register to point to the instruction
            ; segment gotten from the stream, but don't increment the reference
            ; count because stream-get already incremented it.
            (set-register! (sr IS) x #F)
            ;(register-value-set! (sr II) 0)  Already initialized.
            (instruction-interpreter))
          (begin
            (assert (eq? 'stream-empty x))
            ; TODO?: Wait for tasks to become available via stealing from
            ; another processor's PTQ or from the global DTQ.
            ))))

    (define (before-death)
      (registers:cleanup)
      (send* '(terminated)))

    (define (after-death)
      (destroy-message-queue name))

    (values name main before-death after-death))


  (define (instruction-interpreter)

    (define (get-inst id i)
      (let ((inst-word (array-ref id i)))
        (when (fp? inst-word) (processor-exception 'invalid-instruction))
        (fv inst-word)))

    ; Check if this process has been told to terminate.  Done between
    ; instructions to ensure they are not interrupted.
    (unless (memq 'SIGTERM (sigpending))
      ; Execute an instruction.
      (assert (srp? IS))
      (assert (not (srp? II)))
      (let ((id (srv IS)) (i (srv II)))
        (register-value-set! (sr II) (+ 1 i))
        (guard (ex #;((processor-exception? ex) TODO))
          (do-operation (get-inst id i))))
      (instruction-interpreter)))

  ;-----------------------------------------------------------------------------

  ; This library, and its sub-libraries, assume a word size of 64 bits and 16
  ; words per chunk.

  (assert (= 8 word-size))
  (assert (= 16 chunk-wsz))

)
