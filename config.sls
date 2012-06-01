#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library defines configurable values that normally should not be changed
; but can be if you know what you're doing.

(library (vifne config)
  (export
    chunk-size
    id-size
    word-size
    chunk-wsz
    default-storage-file
    register-set-size
    limit-16bit
    exact-integer?
    exact-positive-integer?
    exact-non-negative-integer?
    signed-32bit?
    unsigned-32bit?
    word-integer?
    non-negative-word-integer?)
  (import
    (rnrs base))

  (define chunk-size 128)  ; 1024 bits
  (define id-size 8)       ; 64 bits
  (define word-size id-size)
  (define chunk-wsz (/ chunk-size word-size))

  (define default-storage-file "shared-chunk-storage")

  (define register-set-size (* 16 chunk-wsz))

  ;-----------------------------------------------------------------------------

  (define limit-16bit (expt 2 16))

  (define word-bsz (* 8 word-size))
  (define word-max-unsigned (- (expt 2 word-bsz) 1))
  (define word-max-signed (- (expt 2 (- word-bsz 1)) 1))
  (define word-min-signed (- (expt 2 (- word-bsz 1))))

  (define (exact-integer? x)
    (and (integer? x) (exact? x)))
  (define (exact-positive-integer? x)
    (and (exact-integer? x) (positive? x)))
  (define (exact-non-negative-integer? x)
    (and (exact-integer? x) (not (negative? x))))

  (define (signed-32bit? x)
    (and (exact-integer? x) (<= (- (expt 2 31)) x (- (expt 2 31) 1))))
  (define (unsigned-32bit? x)
    (and (exact-integer? x) (<= 0 x (- (expt 2 32) 1))))

  (define (word-integer? x)
    (and (exact-integer? x) (<= word-min-signed x word-max-unsigned)))
  (define (non-negative-word-integer? x)
    (and (word-integer? x) (not (negative? x))))

  (assert (exact-positive-integer? chunk-size))
  (assert (exact-positive-integer? id-size))
  (assert (exact-positive-integer? word-size))
  (assert (<= id-size word-size))
  (assert (exact-positive-integer? chunk-wsz))
  (assert (< id-size chunk-size))

)
