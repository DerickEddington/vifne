#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

(library (vifne message-queue)
  (export
    create-message-queue
    open-message-queue
    destroy-message-queue
    send
    receive)
  (import
    (rnrs base)
    (rnrs records syntactic)
    (rnrs io ports)
    (rnrs bytevectors)
    (vifne host)
    (vifne main-pid)
    (vifne posix message-queue)
    (vifne posix)
    (vifne foreign))

  (define-record-type message-queue (fields name descriptor buffer))

  (define (fmt s) (string-append "/vifne-" (main-pid-str) "-" s))

  (define buffer-length (message-queue-msgsize_max))

  (define (create-message-queue name)
    (let ((n (fmt name)))
      (make-message-queue n
       (mq_open n (+ O_CREAT O_EXCL O_RDONLY) (+ S_IRUSR S_IWUSR))
       (malloc buffer-length))))

  (define (open-message-queue name)
    (let ((n (fmt name)))
      (make-message-queue n (mq_open n O_WRONLY) (malloc buffer-length))))

  (define (destroy-message-queue name) (mq_unlink (fmt name)))

  (define NT (native-transcoder))

  (define (send mq datum)
    (define (->string x)
      (call-with-string-output-port (lambda (sop) (put-datum sop x))))
    (let* ((bv (string->bytevector (->string datum) NT))
           (l (bytevector-length bv))
           (buf (message-queue-buffer mq)))
      (assert (<= l buffer-length))
      (memcpy buf 0 bv 0 l)
      (mq_send (message-queue-descriptor mq) buf l)))

  (define (receive mq)
    (let* ((buf (message-queue-buffer mq))
           (l (mq_receive (message-queue-descriptor mq) buf buffer-length))
           (bv (make-bytevector l)))
      (memcpy bv 0 buf 0 l)
      (get-datum (open-string-input-port (bytevector->string bv NT)))))

)
