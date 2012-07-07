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
    rv rp? rf r-set!
    II IS R
    srv srp? srf sr-set!
    clear-registers!
    registers:initialize!)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs lists)
    (rnrs records syntactic)
    (vifne config)
    (only (vifne storage) valid-id? f fv fp?)
    (vifne processor exception))

  (define-record-type register (fields (mutable value) (mutable pointer?)))
  (define-record-type special-register (parent register))

  (define (make-register-set size make-reg)
    (let ((v (make-vector size)))
      (do ((i 0 (+ 1 i)))
          ((= size i))
        (vector-set! v i (make-reg 0 #F)))
      v))

  (define register-set (make-register-set register-set-size make-register))

  (define-syntax define-special-registers
    (syntax-rules ()
      ((_ set special ...)
       (begin (define ordered '(special ...))
              (define set (make-register-set (length ordered) make-special-register))
              (define special (- (length ordered) (length (memq 'special ordered))))
              ...))))

  (define-special-registers special-register-set
    IS        ; Instruction Segment
    II        ; Instruction Index
    R         ; Return information
    )

  (define set-register!
    (case-lambda
      ((r x incr?)
       (assert ((if (fp? x) valid-id? non-negative-word-integer?) (fv x)))
       (let ((oldv (register-value r))
             (oldp? (register-pointer? r)))
         (register-value-set! r (fv x))
         (register-pointer?-set! r (fp? x))
         (when (and (fp? x) incr?) (send* `(increment ,(fv x))))
         (when oldp? (send* `(decrement ,oldv)))))
      ((r x)
       (set-register! r x #F))))

  (define-syntax define-register-accessors
    (syntax-rules ()
      ((_ (reg reg-val reg-ptr? reg->field reg-set!) set-vec)
       (begin (define (reg n) (vector-ref set-vec n))
              (define (reg-val n) (register-value (reg n)))
              (define (reg-ptr? n) (register-pointer? (reg n)))
              (define (reg->field n) (f (reg-val n) (reg-ptr? n)))
              (define (reg-set! n . a) (apply set-register! (reg n) a))))))

  (define-register-accessors (r rv rp? rf r-set!) register-set)
  (define-register-accessors (sr srv srp? srf sr-set!) special-register-set)

  (define (register-code? x)
    (and (exact-non-negative-integer? x) (< x register-set-size)))
  (define (special-register-code? x)
    (and (exact-non-negative-integer? x) (< x (vector-length special-register-set))))
  (define (group-mask? x)
    (and (exact-non-negative-integer? x) (<= x #xFFFF)))

  (define (clear-registers!)
    ; Clear the registers, which decrements all referenced chunks' reference
    ; counts.
    (define (clear s) (vector-for-each (lambda (r) (set-register! r (f 0 #F))) s))
    (for-each clear (list register-set special-register-set)))

  ;-----------------------------------------------------------------------------

  (define send*)

  (define (registers:initialize! sender)
    (set! send* sender))

  ;-----------------------------------------------------------------------------

  (assert (exact-positive-integer? register-set-size))
  (assert (<= register-set-size limit-16bit))
  (assert (zero? (mod register-set-size chunk-wsz)))
  (assert (<= (vector-length special-register-set) limit-16bit))

)
