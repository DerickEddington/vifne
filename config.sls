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
    cache-size
    exact-positive-integer?)
  (import
    (rnrs base))

  (define chunk-size 128)  ; 1024 bits
  (define id-size 8)       ; 64 bits
  (define word-size id-size)
  (define chunk-wsz (/ chunk-size word-size))

  (define default-storage-file "shared-chunk-storage")

  (define register-set-size (* 16 chunk-wsz))
  (define cache-size (expt 2 15))

  ;-----------------------------------------------------------------------------

  (define (exact-positive-integer? x) (and (integer? x) (exact? x) (positive? x)))

  (assert (exact-positive-integer? chunk-size))
  (assert (exact-positive-integer? id-size))
  (assert (exact-positive-integer? chunk-wsz))
  (assert (< id-size chunk-size))

)
