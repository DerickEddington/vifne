#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library provides simple command-line arguments processing.

(library (vifne util command-line)
  (export
    define-command-line-arguments
    required-argument-error
    string-non-empty?
    true-string?)
  (import
    (rnrs base)
    (rnrs programs)
    (rnrs lists))

  (define (die msg . a) (apply error (car (command-line)) msg a))

  (define (process-command-line . args-names)
    (let loop ((c (cdr (command-line))) (a '()) (r '()))
      (if (null? c)
        (if (null? r)
          a
          (cons (cons 'rest (reverse r))
                a))
        (if (member (car c) args-names)
          (if (null? (cdr c))
            (die "argument missing value" (car c))
            (loop (cddr c)
                  (cons (cons (car c) (cadr c))
                        a)
                  r))
          (loop (cdr c)
                a
                (cons (car c) r))))))


  (define (get processed name valid? default . funcs)
    (cond
      ((assoc name processed)
       => (lambda (p)
            (let ((v (fold-left (lambda (a f) (f a)) (cdr p) funcs)))
              (if (valid? v)
                v
                (die "invalid argument value" name (cdr p))))))
      (else
       (default))))


  (define-syntax define-command-line-arguments
    (syntax-rules ()
      ((_ (name arg (f ...) pred default) ...)
       (begin
         (define processed (process-command-line 'arg ...))
         (define name (get processed 'arg pred (lambda () default) f ...))
         ...))))


  (define (required-argument-error name)
    (die "missing required argument" name))


  (define (string-non-empty? x) (positive? (string-length x)))

  (define (true-string? s) (or (string=? "true" s) (if (string=? "false" s) #F s)))

)
