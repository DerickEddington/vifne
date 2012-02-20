#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

(library (vifne start)
  (export
    start-emulator!
    stop-emulator!)
  (import
    (rnrs base)
    (rnrs control)
    (vifne posix)
    (vifne storage)
    (vifne message-queue))

  (define number-processors)

  (define (start-emulator! storage-file num-procs)
    (set! number-processors num-procs)
    (apply storage-set! (mmap-storage-file storage-file))
    (PID-set! (getpid))
    ; The above must happen before child processes are forked.
    )

  (define (stop-emulator!)
    (apply munmap (storage-get))
  #;(for-each
     destroy-message-queue
     (cons "SMS"
           (do ((i (- number-processors 1) (- i 1))
                (l '() (cons (string-append "processor" (number->string i)) l)))
               ((negative? i) l)))))

)
