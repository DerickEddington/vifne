#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

(library (vifne processor cache)
  (export
    chunk-ref
    #|new-chunk
    chunk=?
    chunk-set!
    seal-chunk!|#
    get-data-chunk
    get-inst-chunk)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs records syntactic)
    (rnrs hashtables)
    (vifne config)
    (rename (vifne storage) (load-chunk load-chunk*) (store-chunk! store-chunk!*))
    (vifne processor exception))

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
    ; it came from shared storage.  TODO?: Maybe it shouldn't automatically be
    ; immutable, because it seems that persistent storage of mutable chunks is
    ; useful, and that seems safe because immutable-shared safety is enforced by
    ; the spawn instruction disallowing passing of mutable chunks - even if a
    ; mutable chunk is stored, and somehow the task that originally created and
    ; referenced it is suspended and then resumed on a different processor, the
    ; mutable chunk will not be shared between multiple threads, and so there is
    ; no hazard of concurrent access.
    (let ((f&p (load-chunk* id)))
      (if f&p (apply make-chunk id #F f&p) (processor-exception 'guarded))))

  (define (store-chunk! c)
    ; Copy a chunk to shared storage.  The chunk must be mutable because
    ; immutable chunks cannot be mutated.  The chunk becomes immutable because
    ; it is now in shared storage.
    (assert (chunk-mutable? c))
    (store-chunk!* (chunk-id c) (chunk-fields c) (chunk-pointer-flags c))
    (seal-chunk! c))


  ; TODO: When a processor is stopped, should all mutable chunks in the cache be
  ; stored in shared storage?  If not, they'll be lost.  I don't think the
  ; stopping algorithm can wait for a point where there are no mutable chunks in
  ; cache, because some tasks might never reach such a point.  If storing
  ; mutable chunks is allowed (per TODO comment in load-chunk), then mutable
  ; chunks should be stored (as mutable, which will require new support I
  ; think).


  (define data-access-unit (make-vector cache-size #F))
  (define inst-access-unit (make-vector cache-size #F))
  (define data-assoc-unit (make-eqv-hashtable cache-size))
  (define inst-assoc-unit (make-eqv-hashtable cache-size))

  ; The Least-Recently-Used data structure is a mutable double-linked list of
  ; nodes that represent cache locations, and a node lookup table indexed by
  ; cache locations, and a reference to the current least-used node.  The list
  ; is ordered from least- to most-used.  An LRU is updated when a cache
  ; location is accessed, such that the corresponding node advances up the list,
  ; which means "more used", and a new node may become the current least-used.

  (define-record-type LRU (fields table (mutable least)))
  (define-record-type LRU-node (fields index (mutable prev) (mutable next)))

  (define (new-LRU size)
    (let ((t (make-vector size)))
      (do ((i 0 (+ 1 i)))
          ((<= size i))
        (let* ((prev (and (positive? i) (vector-ref t (- i 1))))
               (n (make-LRU-node i prev #F)))
          (vector-set! t i n)
          (when prev (LRU-node-next-set! prev n))))
      (make-LRU t 0)))

  (define data-lru (new-LRU cache-size))
  (define inst-lru (new-LRU cache-size))

  (define (update-LRU! lru i)
    ; Advance a cache location's node in the LRU list, possibly setting a new
    ; least-used location.
    (let* ((i (vector-ref (LRU-table lru) i))
           (in (LRU-node-next i)))
      (when in
        (let ((ip (LRU-node-prev i)))
          (if ip
            (LRU-node-next-set! ip in)
            (LRU-least-set! (LRU-node-index in)))
          (LRU-node-prev-set! i in)
          (LRU-node-next-set! i (LRU-node-next in))
          (LRU-node-prev-set! in ip)
          (LRU-node-next-set! in i)))))

  (define (get-chunk id cache table lru)
    (let ((i (hashtable-ref table id #F)))
      (if i
        (begin (update-LRU! lru i)
               (vector-ref cache i))
        (let* ((c (load-chunk id))
               (i (LRU-least lru))
               (old (vector-ref cache i)))
          (when old
            (when (chunk-mutable? old) (store-chunk! old)) ; TODO: Correct?
            (hashtable-delete! table (chunk-id old)))
          (vector-set! cache i c)
          (hashtable-set! table id i)
          (update-LRU! lru i)
          c))))

  (define (get-data-chunk id) (get-chunk id data-access-unit data-assoc-unit data-lru))
  (define (get-inst-chunk id) (get-chunk id inst-access-unit inst-assoc-unit inst-lru))


  ; TODO: Cache of new mutable chunks ready to be allocated.

)
