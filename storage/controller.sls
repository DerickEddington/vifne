#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library is the shared memory system (SMS) controller.  The SMS services
; requests from (emulated) processors, using message queues to communicate.
; This library must be used only after forking the process for the storage
; controller.

(library (vifne storage controller)
  (export
    start-storage-controller)
  (import
    (rnrs base)
    (rnrs control)
    (vifne message-queue)
    (vifne storage)
    (vifne storage stream)
    (vifne posix redirect)
    (vifne log))

  (define requests)
  (define processors)

  (define (start-storage-controller num-procs)
    (define name "storage-controller")
    (redirect-stdouts name) ; Redirect output before doing anything.
    (set! requests (create-message-queue name))
    (set! processors (make-vector num-procs #F))
    (controller-loop))

  (define (register-proc-mq! mq)
    (do ((i 0 (+ 1 i)))
        ((not (vector-ref processors i))
         (vector-set! processors i mq)
         i)))

  (define (proc-mq i) (vector-ref processors i))


  (define-logger debug storage-controller)

  (define (send* mq x) (debug 'S: x) (send mq x))


  (define (controller-loop)
    (let ((req (receive requests)))

      (define (reply x) (send* (proc-mq (cadr req)) x))

      (debug 'R: req)

      (case (car req)

        ((processor)
         (let ((mq (open-message-queue (cadr req))))
           (send* mq `(processor-id ,(register-proc-mq! mq)))))

        ((allocate)
         (let loop ((count (caddr req)) (a '()))
           (define (done) (reply (if (pair? a) `(chunk-ids . ,a) 'storage-full)))
           (if (positive? count)
             (let ((id (alloc-chunk!)))
               (if id (loop (- count 1) (cons id a)) (done)))
             (done))))

        ((increment)
         (for-each incr-refcount! (cddr req)))

        ((decrement)
         (for-each decr-refcount! (cddr req)))

        ((allocate-stream)
         (let ((h&t (alloc-stream!)))
           (reply (if h&t `(stream-handles . ,h&t) 'storage-full))))

        ((stream-put)
         (let ((x (apply stream-put! (cddr req))))
           (reply (if (symbol? x) x `(notify ,x)))))

        ((stream-get)
         (let ((x (apply stream-get! (cddr req))))
           (reply (if (symbol? x) x `(stream-element . ,x)))))))

    (controller-loop))

)
