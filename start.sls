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
    (rnrs io simple)
    (rnrs control)
    (rnrs programs)
    (rnrs exceptions)
    (vifne posix)
    (vifne posix redirect)
    (vifne exit)
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
            ; Remove the parent process's exit handler from the child process's
            ; exit handlers.
            (remove-exit-handlers!)
            ; Redirect output.
            (redirect-stdouts name)
            ; Register the child process's before-death exit handler.
            (register-exit-handler! (wrap before-death))
            ; Execute the purpose of the child process.
            (main)
            ; Exit the process.  The before-death exit-handler will be run.  The
            ; parent process will detect the child death and run the after-death
            ; clean-up.
            (exit))
          ; The parent process.
          (values pid (wrap after-death))))))


  (define (initialize-libraries! storage-file init-file?)
    (let-values (((addr size) (mmap-storage-file storage-file)))
      (storage-set! addr size init-file? alloc-stream!)
      (main-pid-set! (getpid))
      ; TODO: Should this procedure be returned?  Explicitly unmapping probably
      ; isn't necessary (need to verify in Posix docs), and it's inconsistent
      ; with the child processes not unmapping.
      ; Return the procedure that unmaps the storage file.
      (lambda () (munmap addr size))))


  (define (start-emulator storage-file init-file? num-procs)

    (define (fork-storage-controller)
      (fork-child start-storage-controller num-procs))

    (define (fork-processors sth stt)
      (let loop ((n 0) (pids '()) (cleanups '()))
        (if (< n num-procs)
          (let-values (((p c) (fork-child start-processor n sth stt)))
            (loop (+ 1 n) (cons p pids) (cons c cleanups)))
          (values (reverse pids) (reverse cleanups)))))

    (define processor-pids '())
    (define processor-cleanups '())
    (define storage-controller-pid '())
    (define storage-controller-cleanup '())
    (define unmap-storage-file values)

    (define (stop!)
      ; TODO: Not sure what the policy should be for the parent monitoring the
      ; children, and not sure what a normal emulator exit should be like.

      ; Terminate all the child processes, processors before storage controller.
      ; Their exit handlers should do any cleanup that must be done by them,
      ; which relies on the Scheme system supporting running exit handlers when
      ; SIGTERM is delivered.  TODO: How will the children unmap the storage
      ; file?
      (for-each (lambda (pid) (ignore-exception (kill pid SIGTERM)))
                (append processor-pids storage-controller-pid))
      ; Call the childrens' after-death cleanups.  (The procedures already
      ; are wrapped with ignore-exception.)
      (for-each (lambda (p) (p))
                (append processor-cleanups storage-controller-cleanup))
      ; Unmap the storage file.  TODO: Probably not strictly
      ; necessary, as the OS should automatically do it properly
      ; when the process terminates.
      (ignore-exception (unmap-storage-file)))

    ; If the parent process exits for any reason, need to run stop!.  It's
    ; alright to register this before forking the children, because they will
    ; remove their inherited copies.
    (register-exit-handler! stop!)
    ; Initialize the libraries before the child processes are forked.
    (let ((usf (initialize-libraries! storage-file init-file?)))
      (set! unmap-storage-file usf)
      ; Fork the storage controller process.
      (let-values (((sc-pid sc-cu) (fork-storage-controller)))
        (set! storage-controller-pid (list sc-pid))
        (set! storage-controller-cleanup (list sc-cu))
        ; Fork the processor processes.
        (let-values (((proc-pids proc-cus)
                      (fork-processors (startup-tasks-head) (startup-tasks-tail))))
          (set! processor-pids proc-pids)
          (set! processor-cleanups proc-cus)
          ; TODO?: Setup some to-be-designed text input device stuff?
          ; Wait for a child process to die.
          (let-values (((wpid wstatus) (wait)))
            #;(stop!) ; Not needed, if the Scheme system calls the exit handlers
                      ; when the program exits.
            ; For now, consider a normal exit of the emulator to be when
            ; the storage-controller process exits normally.
            (unless (and (= sc-pid wpid) (zero? wstatus))
              (error 'start-emulator
                     (string-append (if (= sc-pid wpid) "storage-controller"
                                        "processor")
                                    " process terminated abnormally")
                     wpid wstatus)))))))

)
