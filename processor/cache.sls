#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

(library (vifne processor cache)
  (export
    new-chunk
    chunk=?
    chunk-ref
    chunk-set!
    seal-chunk!
    load-chunk
    store-chunk!)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs records syntactic)
    (vifne config)
    (rename (vifne storage) (load-chunk load-chunk*) (store-chunk! store-chunk!*)))

  ; This record type represents a chunk in processor-local memory.  As such, it
  ; contains information not stored in the main shared storage.
  (define-record-type chunk
    (fields id
            (mutable mutable?)
            fields
            pointer-flags))

  (define (new-chunk id)
    (make-chunk id #T (make-vector chunk-wsz 0) (make-vector chunk-wsz #F)))

  (define (chunk=? a b)
    (and (=      (chunk-id a)        (chunk-id b))
         (eqv?   (chunk-mutable? a)  (chunk-mutable? b))
         (equal? (chunk-fields a)    (chunk-fields b))
         (equal? (chunk-pointer-flags a) (chunk-pointer-flags b))))

  ; TODO: It might be better to make chunk-ref and chunk-set! return the vectors
  ; of all the fields and flags, because chunk access should be optimized for
  ; bulk transfer of all the fields to/from multiple registers.

  (define (chunk-ref c i)
    (values (vector-ref (chunk-fields c) i)
            (vector-ref (chunk-pointer-flags c) i)))

  (define (chunk-set! c i v p?)
    (assert (chunk-mutable? c))
    (vector-set! (chunk-fields c) i v)
    (vector-set! (chunk-pointer-flags c) i p?))

  (define (seal-chunk! c) (chunk-mutable?-set! c #F))

  (define (load-chunk id)
    ; Copy a chunk from shared storage.  The returned chunk is immutable because
    ; it came from shared storage.
    (let ((f&p (load-chunk* id))) (and f&p (apply make-chunk id #F f&p))))

  (define (store-chunk! c)
    ; Copy a chunk to shared storage.  The chunk must be mutable because
    ; immutable chunks cannot be mutated.  The chunk becomes immutable because
    ; it is now in shared storage.
    (assert (chunk-mutable? c))
    (store-chunk!* (chunk-id c) (chunk-fields c) (chunk-pointer-flags c))
    (seal-chunk! c))

)
