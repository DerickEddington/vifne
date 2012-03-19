#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library provides simple command-line arguments processing.

(library (vifne util command-line)
  (export
    process-command-line!
    command-line-argument
    string-non-empty?
    true-string?)
  (import
    (rnrs base)
    (rnrs programs)
    (rnrs lists))

  (define (die msg . a) (apply error (car (command-line)) msg a))

  (define processed #F)

  (define (process-command-line! . args-names)
    (set! processed
          (let loop ((c (cdr (command-line)))
                     (a '())
                     (r '()))
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
                      (cons (car c) r)))))))


  (define (get name valid? default . funcs)
    (assert processed)
    (cond
      ((assoc name processed)
       => (lambda (p)
            (let ((v (fold-left (lambda (a f) (f a)) (cdr p) funcs)))
              (if (valid? v)
                v
                (die "invalid argument value" name (cdr p))))))
      (else
       (default))))

  (define-syntax command-line-argument
    (syntax-rules ()
      ((_ name (f ...) pred default)
       (get name pred (lambda () default) f ...))))


  (define (string-non-empty? x) (positive? (string-length x)))

  (define (true-string? s) (or (string=? "true" s) (if (string=? "false" s) #F s)))

)
