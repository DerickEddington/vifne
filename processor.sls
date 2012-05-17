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
    (vifne posix redirect)
    (vifne message-queue)
    (vifne processor array)
    (vifne processor cache)
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
    (redirect-stdouts name) ; Redirect output before doing anything.

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

    ; Initialize the sub-libraries that must be able to message with the storage
    ; controller.  This is done like this to avoid import circles.
    (for-each (lambda (proc) (proc send* receive*))
              (list cache:set-storage-comm!
                    registers:set-storage-comm!
                    operations:set-storage-comm!))

    ; Save the chunk IDs of the startup-tasks stream, for when the processor
    ; stops.
    (set! startup-tasks-head-id sth)
    (set! startup-tasks-tail-id stt)

    ; Start executing instructions.
    (send* `(stream-get ,startup-tasks-head-id #F))
    (let ((x (receive*)))
      (if (list? x)
        (begin (assert (caddr x)) ; ptr? is true
               ; Set the Instruction Segment register to point to the
               ; instruction segment gotten from the stream, but don't increment
               ; the reference count because stream-get already incremented it.
               (set-register! (sr IS) (cadr x) #T #F)
               ;(register-value-set! (sr II) 0)  Already initialized.
               (instruction-interpreter))
        (begin (assert (eq? 'stream-empty x))
               ; TODO?: Wait for tasks to become available via stealing from
               ; another processor's PTQ or from the global DTQ.
               (sleep (expt 2 32))))))


  (define (instruction-interpreter)

    (define (get-inst id i)
      (let-values (((inst-word ptr?) (inst-array-ref id i)))
        (when ptr? (processor-exception 'invalid-instruction))
        inst-word))

    (assert (srp? IS))
    (assert (not (srp? II)))
    (let ((id (srv IS)) (i (srv II)))
      (register-value-set! (sr II) (+ 1 i))
      (guard (ex #;((processor-exception? ex) TODO))
        (do-operation (get-inst id i))))
    (instruction-interpreter))

  ;-----------------------------------------------------------------------------

  ; This library, and its sub-libraries, assume a word size of 64 bits and 16
  ; words per chunk.

  (assert (= 8 word-size))
  (assert (= 16 chunk-wsz))

)
