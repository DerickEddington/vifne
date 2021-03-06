#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library provides means for working with chunks outside the emulator and
; for copying outside chunks into emulator storage files.  It provides the
; ability to manually construct data structures in emulator storage files,
; including the ability to bypass capability-security with magic pointers.

; NOTE: Usage of this library requires that the (vifne storage) library already
; have been initialized, and requires that the emulator is not running.

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
    (vifne storage)
    (vifne util misc))

  ; This type represents a chunk outside an emulator storage file.  The fields
  ; field may contain either: numbers (representing non-pointer values), or
  ; chunk records (representing pointers to those chunks), or magic-pointer
  ; records (representing pointers to fixed chunk-IDs).  Reference cycles
  ; between chunk records are avoided by not providing a means to mutate the
  ; fields.
  (define-record-type chunk (fields fields))
  (define (new-chunk fields)
    ; Takes a variable-length list and fills end with zeros if needed.
    (assert (<= (length fields) chunk-wsz))
    (assert (for-all (lambda (x) (or (word-integer? x)
                                     (chunk? x)
                                     (magic-pointer? x)))
                     fields))
    (make-chunk (list-extend fields chunk-wsz 0)))
  (define (chunk-ref c i) (list-ref (chunk-fields c) i))

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
      (let ((fields (load-chunk id)))
        (unless fields (error 'load "guarded" id))
        (let ((c (make-chunk (map (lambda (f)
                                    (if (fp? f)
                                      (or (hashtable-ref T (fv f) #F)
                                          (recur (fv f)))
                                      (fv f)))
                                  fields))))
          (hashtable-set! T id c)
          c))))


  (define (store! head magic?)
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
        (store-chunk! id
          (map (lambda (v)
                 (define (f* v)
                   (incr-refcount! v)
                   (f v #T))
                 (cond ((chunk? v)
                        (f* (or (hashtable-ref C v #F)
                                (copy! v))))
                       ((magic-pointer? v)
                        (unless magic? (die! "magic pointers not allowed" v))
                        (let ((v (magic-pointer-id v)))
                          (when (hashtable-ref M v #F)
                            (die! "magic pointer allocated for copy" v))
                          (unless (chunk-allocated? v)
                            (die! "magic pointer not allocated" v))
                          (f* v)))
                       (else (f v #F))))
               (chunk-fields c)))
        id))

    (assert (chunk? head))
    ; The head chunk is returned with a reference count of 1, so that it is
    ; considered allocated (necessary for referring to it via a magic pointer).
    ; Remember to correct the reference count after an operation that changes it
    ; to be different than the number of actual references to it.
    (let ((h (copy! head))) (incr-refcount! h) h))

)
