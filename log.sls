#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library provides logging, also useful for debugging.  (Note that the
; output ports are not closed by this library.  It is assumed that the Scheme
; system closes ports when it exits.)

; TODO: Test the portability of this to other Scheme systems.

(library (vifne log)
  (export
    define-logger)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs lists)
    (rnrs io simple)
    (for (vifne log enabled) expand)
    (vifne main-pid))

  (define (file-name key aux)
    (string-append "vifne-" (main-pid-str) "-" (symbol->string key)
                   (apply string-append (map (lambda (a) (string-append "-" a)) aux))
                   ".log"))

  (define (make-port-getter key aux)
    ; (This is implemented like this so that evaluation of the aux expressions
    ; given to define-logger does not occur until the first time they are
    ; needed.  This is important because some forked processes must have some of
    ; these expressions evaluated only after the library initialization and the
    ; forking are already done.)
    (let ((port #F))
      (lambda ()
        (or port
            (begin (set! port (open-output-file (file-name key (map (lambda (a) (a)) aux))))
                   port)))))

  (define (output port objs)
    (fold-left (lambda (i o)
                 (when (positive? i) (display " " port))
                 (write o port)
                 (+ 1 i))
               0 objs)
    (unless (null? objs) (newline port)))

  (define (make-logger key . aux)
    (let ((get-port (make-port-getter key aux)))
      (lambda objs
        (output (get-port) objs))))

  (define-syntax define-logger
    ; (This is implemented like this so that syntax-rules can be used instead of
    ; choosing a procedural macros API.  Note that evaluating general
    ; expand-time expressions is still required.)
    (syntax-rules ()
      ((_ name key aux ...)
       (begin
         (define-syntax helper
           ; Expand-time test.
           (if (member 'key enabled)
             ; Define a procedure to output to the file.
             (syntax-rules ()
               ((_ n k a (... ...))
                (define n (make-logger 'k (lambda () a) (... ...)))))
             ; Disabled, so only define a macro to expand to nothing (#F
             ; should be optimized away).
             (syntax-rules ()
               ((_ n . _)
                (define-syntax n (syntax-rules () ((_ . _) #F)))))))
         (helper name key aux ...)))))

)
