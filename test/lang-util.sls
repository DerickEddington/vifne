#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

(library (vifne test lang-util)
  (export
    prompt
    prompt/nat
    test)
  (import
    (vifne util assembler high-lang)
    (rnrs base)
    (rnrs io simple))

  (define (prompt str)
    (display str (current-error-port))
    (read))

  (define (prompt/nat str)
    (let ((x (prompt str)))
      (assert (and (integer? x) (not (negative? x))))
      x))

  (define test
    (let ((i 0))
      (lambda (reg val)
        (set-immediate r77 val)
        (sub reg reg r77)
        (let ((l (string->symbol (string-append "test" (number->string i) "-success"))))
          (jump-zero reg l)
          (exception i)
          (label l))
        (set! i (+ 1 i)))))

)
