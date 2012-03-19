#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library provides means for working with chunks outside the emulator and
; for copying outside chunks into emulator storage files.  It provides the
; ability to manually construct data structures in emulator storage files,
; including the ability to bypass capability-security with magic pointers.

; NOTE: Usage of this library requires that the (vifne storage) library already
; have been initialized.

(library (vifne util assembler storage)
  (export
    chunk?
    new-chunk
    chunk-ref
    magic-pointer?
    new-magic-pointer
    magic-pointer-id
    load
    store!)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs lists)
    (rnrs hashtables)
    (rnrs records syntactic)
    (vifne config)
    (vifne storage))

  (define word-num-max (- (expt 2 (* 8 word-size)) 1))

  ; This type represents a chunk outside an emulator storage file.  The fields
  ; field may contain either: numbers (representing non-pointer values), or
  ; chunk records (representing pointers to those chunks), or magic-pointer
  ; records (representing pointers to fixed chunk-IDs).  Reference cycles
  ; between chunk records are avoided by not providing a means to mutate the
  ; fields.
  (define-record-type chunk (fields fields))
  (define (new-chunk . a)
    (assert (= chunk-wsz (length a)))
    (assert (for-all (lambda (x) (or (and (integer? x) (exact? x) (<= 0 x word-num-max))
                                     (chunk? x)
                                     (magic-pointer? x)))
                     a))
    (make-chunk (apply vector a)))
  (define (chunk-ref c i) (vector-ref (chunk-fields c) i))

  ; This type represents a synthesized pointer to a chunk.
  (define-record-type magic-pointer (fields id))
  (define (new-magic-pointer id)
    (assert (valid-id? id))
    (make-magic-pointer id))


  (define (load head-id)
    ; Copy a graph of chunks from the mmap'ed storage file, and return the head
    ; of a graph of chunk records that represents it.

    ; This table maps IDs to their corresponding loaded chunk records, to
    ; support chunks referred to more than once in the graph.
    (define T (make-eqv-hashtable))

    (assert (valid-id? head-id))

    (let recur ((id head-id))
      (let ((f&p (load-chunk id)))
        (unless f&p (error 'load "guarded" id))
        (let ((c (make-chunk (vector-map (lambda (f p)
                                           (if p
                                             (or (hashtable-ref T f #F)
                                                 (recur f))
                                             f))
                                         (car f&p) (cadr f&p)))))
          (hashtable-set! T id c)
          c))))


  (define (store! head)
    ; Copy a graph of chunk records to the mmap'ed storage file by allocating
    ; new chunks in it, and return the chunk ID of the graph head.  Magic
    ; pointers are prevented from creating reference cycles, by checking that
    ; they refer to existing/allocated chunks and that they don't refer to
    ; chunks allocated for the copy (this is correct because existing chunks
    ; that aren't for the copy won't point to the chunks for the copy).

    ; This table maps copied chunk records to their corresponding IDs, to
    ; support chunks referred to more than once in the graph.
    (define C (make-eq-hashtable))
    ; This table collects IDs of chunks allocated for the copy, to detect
    ; invalid magic pointers, and to free the chunks if there's an error.
    (define M (make-eqv-hashtable))

    (define (die! msg . a)
      (vector-for-each free-chunk! (hashtable-keys M))
      (apply error 'store! msg a))

    (define (copy! c)
      (let ((id (or (alloc-chunk! 0 0) ; Ref-count = 0
                    (die! "storage full"))))
        (hashtable-set! C c id)
        (hashtable-set! M id #T)
        (let ((f (vector-map (lambda (v)
                               (cond ((chunk? v)
                                      (or (hashtable-ref C v #F)
                                          (copy! v)))
                                     ((magic-pointer? v)
                                      (let ((v (magic-pointer-id v)))
                                        (when (hashtable-ref M v #F)
                                          (die! "magic pointer allocated for copy" v))
                                        (unless (chunk-allocated? v)
                                          (die! "magic pointer not allocated" v))
                                        v))
                                     (else v)))
                             (chunk-fields c)))
              (p (vector-map (lambda (v) (or (chunk? v) (magic-pointer? v)))
                             (chunk-fields c))))
          (store-chunk! id f p)
          (vector-for-each (lambda (f p) (when p (incr-refcount! f))) f p))
        id))

    (assert (chunk? head))
    ; The head chunk is returned with a reference count of 1, so that it is
    ; considered allocated (necessary for referring to it via a magic pointer).
    ; Remember to correct the reference count after an operation that changes it
    ; to be different than the number of actual references to it.
    (let ((h (copy! head))) (incr-refcount! h) h))

)
