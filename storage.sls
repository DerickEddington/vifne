#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

; TODO: Describe the format of chunks and metadata.

(library (vifne storage)
  (export
    guard-tag
    stream-head-tag
    stream-tail-tag
    tagged?
    tags
    id->ptr
    valid-id?
    ref-word
    set-word!
    ref-field
    set-field!
    alloc-chunk!
    chunk-allocated?
    incr-refcount!
    decr-refcount!
    free-chunk!
    load-chunk
    store-chunk!
    storage-set!
    startup-tasks-head
    startup-tasks-tail
    #|deferred-tasks-head
    deferred-tasks-tail|#)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs arithmetic bitwise)
    (vifne config)
    (vifne foreign))

  (define chunk&meta-size (* 2 chunk-size))
  (define meta-wsz 4)
  (define reference-count-field (+ chunk-wsz 0))
  (define tags-field            (+ chunk-wsz 1))
  (define tags-bsz 3)
  (define guard-tag        0)
  (define stream-head-tag  1)
  (define stream-tail-tag  2)
  (define pointer-flags-field   (+ chunk-wsz 2))
  (define next-free-field       (+ chunk-wsz 3))

  (define ref-word  (case word-size ((8) pointer-ref-u64)))
  (define set-word! (case word-size ((8) pointer-set-u64!)))

  (define (ref-field c i)
    (values (ref-word c i)
            (bitwise-bit-set? (ref-word c pointer-flags-field) i)))

  (define (set-field! c i v p?)
    (let ((pfl (ref-word c pointer-flags-field)))
      (let ((oldv (ref-word c i))
            (oldp? (bitwise-bit-set? pfl i)))
        (set-word! c i v)
        (set-word! c pointer-flags-field (bitwise-copy-bit pfl i (if p? 1 0)))
        (when p? (incr-refcount! v))
        (when oldp? (decr-refcount! oldv)))))

  (define (tagged? c bitpos) (bitwise-bit-set? (ref-word c tags-field) bitpos))

  (define (tags . bitsposs)
    (apply bitwise-ior (map (lambda (bp) (bitwise-arithmetic-shift 1 bp)) bitsposs)))


  (define storage-addr)
  (define storage-size)

  (define (storage-set! addr size init? alloc-stream!)
    (set! storage-addr (pointer->integer addr))
    (set! storage-size size)
    (set! control-struct addr)
    ; (Passing alloc-stream! is necessary because it can't be imported from
    ; (vifne storage stream) because circular imports aren't possible.)
    (check-storage! init? alloc-stream!))

  (define (id->ptr id)
    (assert (< id storage-size))
    (integer->pointer (+ storage-addr id)))

  (define (valid-id? x)
    (and (non-negative-word-integer? x) (zero? (mod x chunk&meta-size))))


  ; TODO?: Should the control chunk use its meta chunk like a normal chunk?
  ; I.e. set its ref-count to 1, guard tagged, pointer flags used, next-free
  ; ignored.  This might be useful for future use of the control chunk?

  (define control-struct)  ; Set by storage-set! above.
  (define control-struct-size (* 1 chunk&meta-size))
  (define control-struct-wsz 5)
  (define free-list-field           0)
  (define startup-tasks-head-field  1)
  (define startup-tasks-tail-field  2)
  (define deferred-tasks-head-field 3)
  (define deferred-tasks-tail-field 4)

  (define (free-list) (ref-word control-struct free-list-field))
  (define (free-list-set! id) (set-word! control-struct free-list-field id))

  (define (startup-tasks-head) (ref-word control-struct startup-tasks-head-field))
  (define (startup-tasks-tail) (ref-word control-struct startup-tasks-tail-field))
  #|(define (deferred-tasks-head) (ref-word control-struct deferred-tasks-head-field))
  (define (deferred-tasks-tail) (ref-word control-struct deferred-tasks-tail-field))|#

  (define (check-storage! init? alloc-stream!)
    (define (die msg) (error 'check-storage! msg))
    (if (positive? (free-list))
      (when init? (die "already initialized"))
      (if init?
        ; Initialize the storage.  Should be done only once per file.  The
        ; storage should be all zeros.  Make the chunk following the control
        ; struct be the first free chunk.
        (begin (free-list-set! control-struct-size)
               (let ((s-h&t (alloc-stream!))
                     #;(d-h&t (alloc-stream!)))
                 (assert (and s-h&t #;d-h&t))
                 (set-word! control-struct startup-tasks-head-field (car s-h&t))
                 (set-word! control-struct startup-tasks-tail-field (cadr s-h&t))
                 #|(set-word! control-struct deferred-tasks-head-field (car d-h&t))
                 (set-word! control-struct deferred-tasks-tail-field (cadr d-h&t))|#))
        (die "uninitialized storage"))))


  ; TODO?: Should the default reference count be 1?  Either way, it seems some
  ; situations must be careful to adjust ref-counts or not, based on the default
  ; a chunk was allocated having.  What's the best default?

  (define alloc-chunk!
    (case-lambda
      ((tags refc)
       (let ((id (free-list)))
         (and (< id storage-size)
              (let* ((m (id->ptr id))
                     (next (ref-word m next-free-field)))
                ; Null next means the following chunk is the next (this
                ; semantics supports sparse files).
                (free-list-set! (if (positive? next) next (+ id chunk&meta-size)))
                (set-word! m reference-count-field refc)
                (set-word! m tags-field tags)
                (set-word! m pointer-flags-field 0)
                #;(set-word! m next-free-field 0)
                id))))
      ((tags) (alloc-chunk! tags 1))
      (() (alloc-chunk! 0 1))))

  (define (chunk-allocated? id)
    (positive? (ref-word (id->ptr id) reference-count-field)))

  (define (free-chunk! id)
    (let ((m (id->ptr id)))
      (set-word! m reference-count-field 0)
      #;(set-word! m tags-field 0)
      #;(set-word! m pointer-flags-field 0)
      (set-word! m next-free-field (free-list)))
    (free-list-set! id))

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
        (free-chunk! id))
      c))


  (define (load-chunk id)
    ; Copy a chunk from shared storage.  Disallow if the chunk is guard tagged.
    (let ((m (id->ptr id)))
      (and (not (tagged? m guard-tag))
           (let ((ptrs (ref-word m pointer-flags-field))
                 (f (make-vector chunk-wsz))
                 (p (make-vector chunk-wsz)))
             (do ((i 0 (+ 1 i)))
                 ((= chunk-wsz i))
               (vector-set! f i (ref-word m i))
               (vector-set! p i (bitwise-bit-set? ptrs i)))
             (list f p)))))

  (define (store-chunk! id f p)
    ; Copy a chunk to shared storage.
    (define (ptr? i) (if (vector-ref p i) 1 0))
    (let ((m (id->ptr id)))
      ; The destination must not be guard tagged, because guarded chunks must
      ; not have copies outside shared storage.
      (assert (not (tagged? m guard-tag)))
      (do ((i 0 (+ 1 i))
           (ptrs 0 (bitwise-copy-bit ptrs i (ptr? i))))
          ((= chunk-wsz i) (set-word! m pointer-flags-field ptrs))
        (set-word! m i (vector-ref f i)))))

  ;-----------------------------------------------------------------------------

  ; The chunk size must have enough words for the meta fields.
  (assert (<= meta-wsz chunk-wsz))
  ; The word size must have enough bits for each chunk-field's pointer?-flag.
  (assert (<= chunk-wsz (* 8 word-size)))
  ; The word size must have enough bits for the tags field.
  (assert (<= tags-bsz (* 8 word-size)))
  ; The control-struct size must be a multiple of the chunk&meta size.
  (assert (zero? (mod control-struct-size chunk&meta-size)))
  ; The control-struct size must have enough words for the needed fields.
  (assert (<= (* word-size control-struct-wsz) control-struct-size))

)
