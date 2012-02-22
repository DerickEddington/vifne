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
    (vifne storage)
    (vifne message-queue))

  (define (start-storage-controller num-procs)
    (set! requests (create-message-queue "storage-controller"))
    (set! processors (make-vector num-procs #F))
    (controller-loop))

  (define requests)
  (define processors)

  (define (register-proc-mq! mq)
    (let ((i (do ((i 0 (+ 1 i))) ((not (vector-ref processors i)) i))))
      (vector-set! processors i mq)
      i))

  (define (proc-mq i) (vector-ref processors i))

  (define (controller-loop)
    (let ((req (receive requests)))
      (case (car req)
        ((allocate)
         (let loop ((count (cadr req)) (a '()))
           (define (done) (send (proc-mq (caddr req)) `(chunk-ids . ,a)))
           (if (positive? count)
             (let ((id (alloc-id!)))
               (if id (loop (- count 1) (cons id a)) (done)))
             (done))))
        ((increment)
         (for-each incr-refcount! (cdr req)))
        ((decrement)
         (for-each decr-refcount! (cdr req)))
        ((processor)
         (let ((mq (open-message-queue (cadr req))))
           (send mq `(processor-id ,(register-proc-mq! mq)))))))
    (controller-loop))

)
