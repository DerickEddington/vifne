#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

; TODO: Describe the format of chunks and metadata in the storage file.

(library (vifne storage)
  (export
    new-chunk
    chunk=?
    chunk-ref
    chunk-set!
    seal-chunk!
    load-chunk
    store-chunk!
    alloc-id!
    incr-refcount!
    decr-refcount!
    storage-set!
    storage-get
    check-storage!)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs records syntactic)
    (rnrs arithmetic bitwise)
    (vifne config)
    (vifne foreign))

  (define chunk&meta-size (* 2 chunk-size))
  (define chunk-wsz (/ chunk-size id-size))
  (define meta-wsz 3)
  (define reference-count-field (+ chunk-wsz 0))
  (define pointer-flags-field   (+ chunk-wsz 1))
  (define next-free-field       (+ chunk-wsz 2))

  (define ref-word  (case id-size ((8) pointer-ref-u64)))
  (define set-word! (case id-size ((8) pointer-set-u64!)))


  ; This record type represents a chunk in processor-local memory.  As such, it
  ; contains information not stored in the main shared storage.
  (define-record-type chunk
    (fields id
            fields
            pointers?
            (mutable mutable?)))

  (define (new-chunk id)
    (make-chunk id (make-vector chunk-wsz 0) (make-vector chunk-wsz #F) #T))

  (define (chunk=? a b)
    (and (=      (chunk-id a)        (chunk-id b))
         (equal? (chunk-fields a)    (chunk-fields b))
         (equal? (chunk-pointers? a) (chunk-pointers? b))
         (eqv?   (chunk-mutable? a)  (chunk-mutable? b))))

  (define (chunk-ref c i)
    (values (vector-ref (chunk-fields c) i)
            (vector-ref (chunk-pointers? c) i)))

  (define (chunk-set! c i v p?)
    (assert (chunk-mutable? c))
    (vector-set! (chunk-fields c) i v)
    (vector-set! (chunk-pointers? c) i p?))

  (define (seal-chunk! c) (chunk-mutable?-set! c #F))


  (define storage-addr)
  (define storage-size)

  (define (storage-set! p s)
    (set! storage-addr (pointer->integer p))
    (set! storage-size s)
    (set! control-struct p))

  (define (storage-get)
    (list (integer->pointer storage-addr)
          storage-size))

  (define (id->ptr id) (integer->pointer (+ storage-addr id)))


  (define (load-chunk id)
    ; Copy a chunk from shared storage.  The returned chunk is immutable
    ; because it came from shared storage.
    (let ((m (id->ptr id))
          (c (new-chunk id)))
      (let ((f (chunk-fields c))
            (p (chunk-pointers? c))
            (ptrs (ref-word m pointer-flags-field)))
        (do ((i 0 (+ 1 i)))
            ((= chunk-wsz i))
          (vector-set! f i (ref-word m i))
          (vector-set! p i (bitwise-bit-set? ptrs i)))
        (seal-chunk! c)
        c)))

  (define (store-chunk! c)
    ; Copy a chunk to shared storage.  The chunk must be mutable because
    ; immutable chunks cannot be mutated.  The chunk becomes immutable because
    ; it is now in shared storage.
    (assert (chunk-mutable? c))
    (let ((m (id->ptr (chunk-id c)))
          (f (chunk-fields c))
          (p (chunk-pointers? c)))
      (define (ptr? i) (if (vector-ref p i) 1 0))
      (do ((i 0 (+ 1 i))
           (ptrs 0 (bitwise-copy-bit ptrs i (ptr? i))))
          ((= chunk-wsz i) (set-word! m pointer-flags-field ptrs))
        (set-word! m i (vector-ref f i)))
      (seal-chunk! c)))

  ;-----------------------------------------------------------------------------

  (define control-struct)  ; Set by storage-set! above.
  (define control-struct-size (* 1 chunk&meta-size))
  (define free-list-field 0)

  (define (free-list) (ref-word control-struct free-list-field))
  (define (free-list-set! id) (set-word! control-struct free-list-field id))

  (define (check-storage! init?)
    (define (die msg) (error 'check-storage! msg))
    (if (positive? (free-list))
      (when init? (die "already initialized"))
      (if init?
        ; Initialize the storage.  Should be done only once per file.  The
        ; storage should be all zeros.  Make the chunk following the control
        ; struct be the first free chunk.
        (free-list-set! control-struct-size)
        (die "uninitialized storage"))))


  (define (alloc-id!)
    (let ((id (free-list)))
      (unless (< id storage-size) (error 'alloc-id! "storage full"))
      (let* ((m (id->ptr id))
             (next (ref-word m next-free-field)))
        ; Null next means the following chunk is the next.
        (free-list-set! (if (positive? next) next (+ id chunk&meta-size)))
        (set-word! m reference-count-field 1)
        #;(set-word! m pointer-flags-field 0)
        #;(set-word! m next-free-field 0))
      id))

  (define (free-id! id)
    (let ((m (id->ptr id)))
      #;(set-word! m reference-count-field 0)
      #;(set-word! m pointer-flags-field 0)
      (set-word! m next-free-field (free-list))
      (free-list-set! id)))


  (define (adjust-refcount! n p)
    (let ((c (+ n (ref-word p reference-count-field))))
      (set-word! p reference-count-field c)
      c))

  (define (incr-refcount! id) (adjust-refcount! 1 (id->ptr id)))

  (define (decr-refcount! id)
    (let* ((m (id->ptr id))
           (c (adjust-refcount! -1 m)))
      (when (zero? c)
        (let ((ptrs (ref-word m pointer-flags-field)))
          (do ((i 0 (+ 1 i)))
              ((= chunk-wsz i))
            (when (bitwise-bit-set? ptrs i) (decr-refcount! (ref-word m i)))))
        (free-id! id))
      c))

  ;-----------------------------------------------------------------------------

  (define (exact-non-negative-integer? x) (and (integer? x) (exact? x) (not (negative? x))))

  (assert (exact-non-negative-integer? chunk-wsz))
  ; The chunk size must have enough words for the meta fields.
  (assert (<= meta-wsz chunk-wsz))
  ; The word size must have enough bits for each chunk-field's pointer?-flag.
  (assert (<= chunk-wsz (* 8 id-size)))
  ; The control-struct size must be a multiple of the chunk&meta size.
  (assert (exact-non-negative-integer? (/ control-struct-size chunk&meta-size)))

)
