#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO.  It must only be used after forking a process for a
; processor.

(library (vifne processor registers)
  (export
    register-code?
    special-register-code?
    group-mask?
    set-register!
    register-value-set!
    rv rp? r-set!
    II IS
    sr srv srp? sr-copy!
    registers:cleanup
    registers:set-storage-comm!)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs lists)
    (rnrs records syntactic)
    (vifne config)
    (only (vifne storage) valid-id?)
    (vifne processor exception))

  (define-record-type register (fields (mutable value) (mutable pointer?)))
  (define-record-type special-register (parent register) (fields validator))

  (define register-set
    (let ((v (make-vector register-set-size)))
      (do ((i 0 (+ 1 i)))
          ((= register-set-size i))
        (vector-set! v i (make-register 0 #F)))
      v))

  (define-syntax define-special-registers
    (syntax-rules ()
      ((_ set (special validator) ...)
       (begin (define set (vector (make-special-register 0 #F validator) ...))
              (define ordered '(special ...))
              (define special (- (length ordered) (length (memq 'special ordered))))
              ...))))

  (define pointer? register-pointer?)
  (define (non-pointer? r) (not (pointer? r)))

  (define-special-registers special-register-set
    (IS     pointer?)           ; Instruction Segment
    (II     non-pointer?)       ; Instruction Index
    )

  (define (set-register! r v p? incr?)
    (assert ((if p? valid-id? non-negative-word-integer?) v))
    (let ((oldv (register-value r))
          (oldp? (register-pointer? r)))
      (register-value-set! r v)
      (register-pointer?-set! r p?)
      (when (and p? incr?) (send* `(increment ,v)))
      (when oldp? (send* `(decrement ,oldv)))))

  (define (r n) (vector-ref register-set n))
  (define (rv n) (register-value (r n)))
  (define (rp? n) (register-pointer? (r n)))
  (define (r-set! n v p? incr?) (set-register! (r n) v p? incr?))

  (define (sr n) (vector-ref special-register-set n))
  (define (srv n) (register-value (sr n)))
  (define (srp? n) (register-pointer? (sr n)))
  (define (sr-copy! s x)
    (let ((s (sr s)) (x (r x)))
      (unless ((special-register-validator s) x)
        (processor-exception 'invalid-special-register-value))
      (set-register! s (register-value x) (register-pointer? x) #T)))

  (define (register-code? x)
    (and (exact-non-negative-integer? x) (< x register-set-size)))
  (define (special-register-code? x)
    (and (exact-non-negative-integer? x) (< x (vector-length special-register-set))))
  (define (group-mask? x)
    (and (exact-non-negative-integer? x) (<= x #xFFFF)))

  (define (registers:cleanup)
    ; Clear the registers, which decrements all referenced chunks' reference
    ; counts.
    (define (clear s) (vector-for-each (lambda (r) (set-register! r 0 #F #F)) s))
    (for-each clear (list register-set special-register-set)))

  ;-----------------------------------------------------------------------------

  (define send*)

  (define (registers:set-storage-comm! sender receiver)
    (set! send* sender))

  ;-----------------------------------------------------------------------------

  (assert (exact-positive-integer? register-set-size))
  (assert (<= register-set-size limit-16bit))
  (assert (zero? (mod register-set-size chunk-wsz)))
  (assert (<= (vector-length special-register-set) limit-16bit))

)
