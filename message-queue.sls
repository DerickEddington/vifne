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
    (rnrs exceptions)
    (rnrs io ports)
  #;(rnrs io simple)
    (rnrs bytevectors)
    (vifne host)
    (vifne main-pid)
    (vifne posix message-queue)
    (vifne posix)
    (vifne foreign))

  (define-record-type message-queue (fields name descriptor buffer))

  (define (fmt s) (string-append "/vifne-" (main-pid-str) "-" s))

  (define buffer-length (message-queue-msgsize_max))

  (define-syntax with-buffer
    (syntax-rules ()
      ((_ v . body)
       (let ((v (malloc buffer-length)))
         (with-exception-handler
           (lambda (ex)
             (free v)
             (raise ex))
           (lambda () . body))))))

  (define (oflag mode)
    (case mode
      ((read) O_RDONLY)
      ((write) O_WRONLY)
      ((read/write) O_RDWR)))

  (define (create-message-queue* name mode buf)
    (let ((n (fmt name)))
      (make-message-queue n
       (mq_open n (+ O_CREAT O_EXCL (oflag mode)) (+ S_IRUSR S_IWUSR))
       buf)))

  (define (create-message-queue name)
    (with-buffer buf (create-message-queue* name 'read buf)))

  (define (open-message-queue* name mode buf)
    (let ((n (fmt name)))
      (make-message-queue n (mq_open n (oflag mode)) buf)))

  (define (open-message-queue name)
    (with-buffer buf (open-message-queue* name 'write buf)))

  (define (destroy-message-queue name) (mq_unlink (fmt name)))

  (define NT (native-transcoder))

  (define (send mq datum)
    (define (->string x)
      (call-with-string-output-port (lambda (sop) (put-datum sop x))))
  #;(begin (write mq) (newline)
           (write datum) (newline))
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
