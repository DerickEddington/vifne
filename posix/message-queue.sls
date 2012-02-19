#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

(library (vifne posix message-queue)
  (export
    mq_open
    mq_send
    mq_receive
    mq_close
    mq_unlink)
  (import
    (rnrs base)
    (rnrs control)
    (vifne host)
    (rename (vifne foreign) (foreign foreign*))
    (vifne posix))

  (define mq-lib (foreign-library message-queue-library-name))

  (define-syntax foreign (syntax-rules () ((_ x ...) (foreign* x ... mq-lib))))


  (define mq_open-raw (foreign ("mq_open" pointer       ; char*            name
                                          signed-int    ; int              oflag
                                          unsigned-int  ; mode_t           mode
                                          pointer)      ; struct mq_attr*  attr
                               signed-int))  ; returns mqd_t

  (define mq_open
    (case-lambda
      ((name oflag mode)
       (let* ((s (string->c-str name))
              (mqd (mq_open-raw s oflag mode NULL)))
         (free s)
         (when (negative? mqd) (error/errno 'mq_open name oflag mode))
         mqd))
      ((name oflag)
       (mq_open name oflag 0))))


  (define mq_send-raw (foreign ("mq_send" signed-int     ; mqd_t     mqdes
                                          pointer        ; char*     msg_ptr
                                          unsigned-long  ; size_t    msg_len
                                          unsigned-int)  ; unsigned  msg_prio
                               signed-int))  ; returns int

  (define (mq_send mqd p len)
    (unless (zero? (mq_send-raw mqd p len 0))
      (error/errno 'mq_send mqd p len)))


  (define mq_receive-raw (foreign ("mq_receive" signed-int     ; mqd_t      mqdes
                                                pointer        ; char*      msg_ptr
                                                unsigned-long  ; size_t     msg_len
                                                pointer)       ; unsigned*  msg_prio
                                  signed-long))  ; returns ssize_t

  (define (mq_receive mqd p len)
    (let ((s (mq_receive-raw mqd p len NULL)))
      (when (negative? s) (error/errno 'mq_receive mqd p len))
      s))


  (define mq_close-raw (foreign ("mq_close" signed-int)  ; mqd_t  mqdes
                                signed-int))  ; returns int

  (define (mq_close mqd) (unless (zero? (mq_close-raw mqd)) (error/errno 'mq_close mqd)))


  (define mq_unlink-raw (foreign ("mq_unlink" pointer)  ; char*  name
                                 signed-int))  ; returns int

  (define (mq_unlink name)
    (let* ((s (string->c-str name))
           (r (mq_unlink-raw s)))
      (free s)
      (unless (zero? r) (error/errno 'mq_unlink name))))

)
