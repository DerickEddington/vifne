#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library defines configurable values that normally should not be changed
; but can be if you know what you're doing.

(library (vifne config)
  (export
    chunk-size
    id-size
    default-storage-file)
  (import
    (rnrs base))

  (define chunk-size 128)  ; 1024 bits
  (define id-size 8)       ; 64 bits

  (define default-storage-file "shared-chunk-storage")

  ;-----------------------------------------------------------------------------

  (define (exact-non-negative-integer? x) (and (integer? x) (exact? x) (not (negative? x))))

  (assert (exact-non-negative-integer? chunk-size))
  (assert (exact-non-negative-integer? (/ chunk-size 8)))
  (assert (exact-non-negative-integer? id-size))
  (assert (exact-non-negative-integer? (/ id-size 8)))
  (assert (exact-non-negative-integer? (/ chunk-size id-size)))
  (assert (< id-size chunk-size))

)
