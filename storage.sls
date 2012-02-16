#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

; TODO: Auxiliary information in chunks that indicates the type and format of
;       chunk contents.

(library (vifne storage)
  (export
    make-chunk
    chunk-mutable?
    chunk-ref
    chunk-set!
    load-chunk
    store-chunk
    storage-set!
    storage-get)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs records syntactic)
    (vifne config)
    (vifne foreign))

  (define chunk-words (/ chunk-size id-size))
  (define ref-word  (case id-size ((8) pointer-ref-u64)))
  (define set-word! (case id-size ((8) pointer-set-u64!)))

  ; This record type represents a chunk in processor-local memory.  As such, it
  ; contains information not stored in the main shared storage.
  (define-record-type chunk (fields fields mutable?))

  (define (chunk-ref c i) (vector-ref (chunk-fields c) i))
  (define (chunk-set! c i v) (vector-set! (chunk-fields c) i v))

  (define storage-addr)
  (define storage-size)
  (define (storage-set! p s)
    (set! storage-addr (pointer->integer p))
    (set! storage-size s))
  (define (storage-get)
    (list (integer->pointer storage-addr)
          storage-size))

  (define (id->ptr id) (integer->pointer (+ storage-addr id)))

  (define (load-chunk id)
    ; Copy a chunk from shared storage.  The returned chunk is immutable
    ; because it came from shared storage.
    (let ((p (id->ptr id))
          (v (make-vector chunk-words)))
      (do ((i 0 (+ 1 i)))
          ((= chunk-words i) (make-chunk v #F))
        (vector-set! v i (ref-word p i)))))

  (define (store-chunk id c)
    (let ((p (id->ptr id))
          (v (chunk-fields c)))
      (do ((i 0 (+ 1 i)))
          ((= chunk-words i))
        (set-word! p i (vector-ref v i)))))

)
