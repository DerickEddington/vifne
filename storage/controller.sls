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
    (vifne log))

  (define requests)
  (define processors)


  (define (start-storage-controller num-procs)
    (define name "storage-controller")
    (define (main)
      (set! requests (create-message-queue name))
      (set! processors (make-vector num-procs 'unregistered))
      (controller-loop))
    (define (before-death)
      (assert (all-terminated?)))
    (define (after-death)
      (destroy-message-queue name))
    (values name main before-death after-death))


  (define (register-proc-mq! mq)
    (do ((i 0 (+ 1 i)))
        ((eq? 'unregistered (vector-ref processors i))
         (vector-set! processors i mq)
         i)))

  (define (proc-mq i) (vector-ref processors i))

  (define (unregister! i) (vector-set! processors i #F))

  (define (all-terminated?)
    (not (let loop ((i 0))
           (or (vector-ref processors i)
               (and (< (+ 1 i) (vector-length processors))
                    (loop (+ 1 i)))))))


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

        ((terminated)
         (unregister! (cadr req)))

        ((allocate)
         (let ((id (alloc-chunk!)))
           (reply (if id `(chunk-id ,id) 'storage-full))))

        ((increment)
         (incr-refcount! (caddr req)))

        ((decrement)
         (decr-refcount! (caddr req)))

        ((allocate-stream)
         (let ((h&t (alloc-stream!)))
           (reply (if h&t `(stream-handles . ,h&t) 'storage-full))))

        ((stream-put)
         (let ((x (apply stream-put! (cddr req))))
           (reply (if (symbol? x) x `(notify ,x)))))

        ((stream-get)
         (let ((x (apply stream-get! (cddr req))))
           (reply (if (symbol? x) x `(stream-element . ,x)))))

        (else (assert #F))))

    (unless (all-terminated?)
      (controller-loop)))

)
