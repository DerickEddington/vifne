#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

(library (vifne start)
  (export
    mmap-storage-file
    initialize-libraries!
    start-emulator)
  (import
    (rnrs base)
    (rnrs lists)
    (rnrs io simple)
    (rnrs control)
    (rnrs programs)
    (rnrs exceptions)
    (vifne posix)
    (vifne posix redirect)
    (vifne posix signals)
    (vifne exit)
    (vifne message-queue)
    (vifne main-pid)
    (vifne storage)
    (vifne storage stream)
    (vifne storage controller)
    (vifne processor))


  (define (mmap-storage-file file)
    (let* ((fd (open file O_RDWR))
           (size (file-size file))
           (p (mmap NULL size (+ PROT_READ PROT_WRITE) MAP_SHARED fd 0)))
      (close fd)
      (values p size)))


  (define (display-ignored-exception ex)
    (let ((cep (current-error-port)))
      (display "Ignored exception:\n" cep)
      (write ex cep) (newline cep)))

  (define-syntax ignore-exception
    (syntax-rules ()
      ((_ expr ...)
       (guard (ex (else (display-ignored-exception ex)))
         expr ...))))


  (define (fork-child helper . args)
    ; To clean-up resources used by a child process, a helper supplies two
    ; procedures: one that does any clean-up that must be done by the child
    ; before it dies, and another that does any clean-up that can be done by the
    ; parent after the child has died.
    (define (wrap p)
      (lambda () (when p (ignore-exception (p)))))
    (let-values (((name main before-death after-death)
                  (apply helper args)))
      ; Do the fork.
      (let ((pid (fork)))
        (if (zero? pid)
          ; The child process.
          (begin
            ; Redirect output.
            (redirect-stdouts name)
            ; Register the child process's before-death exit handler.
            (exit-handler (wrap before-death))
            ; Disable interrupts, to protect all the various critical sections
            ; in the child processes, including the exit handlers.
            (block-signals)
            ; Execute the purpose of the child process.
            (main)
            ; Exit the process.  The before-death exit-handler will be run.  The
            ; parent process will detect the child death and run the after-death
            ; clean-up.
            (exit))
          ; The parent process.
          (values pid (wrap after-death))))))


  (define (initialize-libraries! storage-file init-file?)
    (main-pid-set! (getpid))
    (let-values (((addr size) (mmap-storage-file storage-file)))
      (storage-set! addr size init-file? alloc-stream!)))


  (define (start-emulator storage-file init-file? num-procs)

    (define (fork-storage-controller)
      (fork-child start-storage-controller num-procs))

    (define (fork-processors sth rth rtt)
      (let loop ((n 0) (pids '()) (cleanups '()))
        (if (< n num-procs)
          (let-values (((p c) (fork-child start-processor n sth rth rtt)))
            (loop (+ 1 n) (cons p pids) (cons c cleanups)))
          (values (reverse pids) (reverse cleanups)))))

    (define processor-pids '())
    (define processor-cleanups '())
    (define storage-controller-pid #F)
    (define storage-controller-cleanup #F)

    (define (stop!)
      (define (term pid) (ignore-exception (kill pid SIGTERM)))
      (define (wait pid) (ignore-exception (waitpid pid)))
      ; Terminate the child processes.  Their exit handlers should do any
      ; before-death clean-up that must be done by them.  The storage controller
      ; must not be terminated until after the processors have completed their
      ; clean-up.  Also, call the processes' after-death clean-ups, after they
      ; are terminated.
      (for-each term processor-pids)
      ; Wake up any processor processes blocked in activate-ready-task, so they
      ; can clean-up and terminate.  Only need to send one message, because a
      ; receiver (if there is one) will send another message to wake up possible
      ; other blocked processes.  This design avoids filling the message queue,
      ; which would block this process and deadlock the emulator.
      (ignore-exception (send (open-message-queue "waiting-processors") 'stop))
      (for-each wait processor-pids)
      (for-each (lambda (p) (p)) processor-cleanups)
      (when storage-controller-pid
        #;(term storage-controller-pid) ; It exits when all processors have.
        (wait storage-controller-pid)
        (storage-controller-cleanup)))

    ; If the parent process exits for any reason, including signal delivery,
    ; need to run stop!.  It's alright to register this before forking the
    ; children, because they will replace with their own.
    (exit-handler stop!)

    ; Initialize the libraries before the child processes are forked.
    (initialize-libraries! storage-file init-file?)

    ; Fork the storage controller process.
    (let-values (((pid cu) (fork-storage-controller)))
      (set! storage-controller-pid pid)
      (set! storage-controller-cleanup cu))

    ; Fork the processor processes.
    (let-values (((pids cus)
                  (fork-processors (startup-tasks-head)
                                   (ready-tasks-head) (ready-tasks-tail))))
      (set! processor-pids pids)
      (set! processor-cleanups cus))

    ; TODO?: Setup some to-be-designed text input device stuff?

    ; Wait for a child process to die, and decide what to do about it.
    (let loop ((alive (cons storage-controller-pid processor-pids)))
      (unless (null? alive)
        (let-values (((wpid wstatus) (waitpid -1)))
          (when (or (not (zero? wstatus))
                    (and (= storage-controller-pid wpid)
                         (not (equal? (list storage-controller-pid) alive))))
            (error 'start-emulator
                   (string-append (if (= storage-controller-pid wpid)
                                    "storage-controller"
                                    "processor")
                                  " process terminated abnormally")
                   wpid wstatus))
          (loop (remv wpid alive))))))

)
