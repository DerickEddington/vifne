#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

(library (vifne start)
  (export
    initialize-libraries!
    start-emulator!)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs programs)
    (rnrs exceptions)
    (rnrs conditions)
    (vifne posix)
    (vifne main-pid)
    (vifne storage)
    (vifne storage stream)
    (vifne storage controller)
    (vifne processor)
    (vifne message-queue))

  (define (mmap-storage-file file size)
    (let* ((fd (open file O_RDWR))
           (p (mmap NULL size (+ PROT_READ PROT_WRITE) MAP_SHARED fd 0)))
      (close fd)
      p))

  (define-syntax fork*
    (syntax-rules ()
      ((_ expr ...)
       (let ((pid (fork)))
         (if (zero? pid)
           (begin expr ... (exit))
           pid)))))

  (define-syntax ignore-error
    (syntax-rules ()
      ((_ expr ...)
       (guard (ex ((error? ex))) expr ...))))

  (define (initialize-libraries! storage-file init-file?)
    (let* ((size (file-size storage-file))
           (addr (mmap-storage-file storage-file size)))
      (storage-set! addr size init-file? alloc-stream!)
      (main-pid-set! (getpid))
      ; Return the procedure that cleans-up the initialization.
      (lambda () (munmap addr size))))

  (define (start-emulator! storage-file init-file? num-procs)
    (let ((clean-up (initialize-libraries! storage-file init-file?)))
      ; The above must happen before the child processes are forked.
      (let ((sc-pid (fork* (start-storage-controller num-procs)))
            (sc-mq "storage-controller"))
        (let loop ((n 0) (proc-pids '()) (proc-mqs '()))
          (if (< n num-procs)
            (loop (+ 1 n)
                  (cons (fork* (start-processor n (startup-tasks-head) (startup-tasks-tail)))
                        proc-pids)
                  (cons (string-append "processor" (number->string n)) proc-mqs))
            (begin
              ; TODO?: Setup the text input device stuff?
              ; Return the procedure that waits for the emulator to stop.
              (lambda ()
                (define (stop!)
                  (for-each (lambda (c) (ignore-error (kill c SIGTERM)))
                            (cons sc-pid proc-pids))
                  (for-each (lambda (mq) (ignore-error (destroy-message-queue mq)))
                            (cons sc-mq proc-mqs))
                  (ignore-error (clean-up)))
                (let-values (((wpid wstatus) (wait)))
                  (stop!)
                  (unless (and (= sc-pid wpid) (zero? wstatus))
                    (error #F (string-append
                               ; TODO: Identify what processor process terminated
                               ; abnormally, in a way that can support future other types
                               ; of processes too.
                               (if (= sc-pid wpid) "storage-controller" "processor")
                               " process terminated abnormally")
                           wpid wstatus))))))))))

)
