#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO.  It must only be used after forking a process for a
; processor, after the storage is properly initialized.

(library (vifne processor cache)
  (export
    new-chunk
    chunk-ref
    chunk-set!
    store-chunk!
    get-data-chunk
    get-inst-chunk
    cache:cleanup
    cache:set-storage-comm!)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs records syntactic)
    (rnrs hashtables)
    (vifne config)
    (prefix (only (vifne storage) load-chunk store-chunk!) storage:)
    (vifne processor exception))

  ; This record type represents a chunk in processor-local memory.
  (define-record-type chunk (fields id fields pointer-flags))

  (define (new-chunk id)
    (make-chunk id (make-vector chunk-wsz 0) (make-vector chunk-wsz #F)))

  (define (chunk-ref c i)
    (values (vector-ref (chunk-fields c) i)
            (vector-ref (chunk-pointer-flags c) i)))

  (define (chunk-set! c i v p?)
    (vector-set! (chunk-fields c) i v)
    (vector-set! (chunk-pointer-flags c) i p?))

  ; TODO: Note about why messaging with the storage controller isn't necessary
  ; for this loading and storing - concurrent access will not happen, because
  ; TODO, so it's an optimization to access directly instead of messaging.
  ; Unlike an actual F.B. computer which would message with the storage
  ; controller.

  (define (load-chunk id)
    ; Copy a chunk from shared storage.
    (let ((f&p (storage:load-chunk id)))
      (if f&p (apply make-chunk id f&p) (processor-exception 'guarded))))

  (define (store-chunk! c)
    ; Copy a chunk to shared storage.
    (storage:store-chunk! (chunk-id c) (chunk-fields c) (chunk-pointer-flags c)))

  (define data-access-unit (make-vector cache-size #F))
  (define inst-access-unit (make-vector cache-size #F))
  (define data-assoc-unit (make-eqv-hashtable cache-size))
  (define inst-assoc-unit (make-eqv-hashtable cache-size))

  (define (cache:cleanup)
    ; Clear the cache.  First, decrement all cached chunks' reference counts.
    (define (clear au)
      (do ((i 0 (+ 1 i)))
          ((= (vector-length au) i))
        (let ((c (vector-ref au i)))
          (when c
            (send* `(decrement ,(chunk-id c)))
            (vector-set! au i #F)))))
    (for-each clear (list data-access-unit inst-access-unit)))

  ; The Least-Recently-Used data structure is a mutable double-linked list of
  ; nodes that represent cache locations, and a node lookup table indexed by
  ; cache locations, and references to the current least- and most-recently-used
  ; nodes.  The list is ordered from least- to most-recently-used.  An LRU is
  ; updated when a cache location is accessed, such that the corresponding node
  ; becomes the most-recently-used, and a new node may become the
  ; least-recently-used.

  (define-record-type LRU (fields table (mutable least) (mutable most)))
  (define-record-type LRU-node (fields index (mutable prev) (mutable next)))

  (define (new-LRU)
    (let ((t (make-vector cache-size)))
      (do ((i 0 (+ 1 i)))
          ((<= cache-size i))
        (let* ((prev (and (positive? i) (vector-ref t (- i 1))))
               (n (make-LRU-node i prev #F)))
          (vector-set! t i n)
          (when prev (LRU-node-next-set! prev n))))
      (make-LRU t 0 (vector-ref t (- (vector-length t) 1)))))

  (define data-lru (new-LRU))
  (define inst-lru (new-LRU))

  (define (update-LRU! lru i)
    ; Make a cache location's node be the most-recently-used in the LRU list.
    ; Also possibly set a new least-recently-used location.
    (let* ((i (vector-ref (LRU-table lru) i))
           (n (LRU-node-next i)))
      (when n
        (let ((p (LRU-node-prev i)))
          (if p
            (LRU-node-next-set! p n)
            (LRU-least-set! lru (LRU-node-index n)))
          (LRU-node-prev-set! n p)
          (let ((m (LRU-most lru)))
            (LRU-node-prev-set! i m)
            (LRU-node-next-set! i #F)
            (LRU-node-next-set! m i)
            (LRU-most-set! lru i))))))

  (define (cache-chunk c cache table lru)
    (let* ((i (LRU-least lru))
           (old (vector-ref cache i)))
      ; Reference counts must be incremented when chunks are cached, to prevent
      ; IDs from being allocated if they're still in any processor's
      ; cache. TODO: More about the more complicated problems that would
      ; otherwise happen - a chunk's ref-count goes to zero, gets freed, gets
      ; re-allocated, but cache still has old chunk with same ID and thinks
      ; that's valid to return for the new chunk, which would be incorrect.
      (send* `(increment ,(chunk-id c)))
      (when old
        (hashtable-delete! table (chunk-id old))
        (send* `(decrement ,(chunk-id old))))
      (vector-set! cache i c)
      (hashtable-set! table (chunk-id c) i)
      (update-LRU! lru i)))

  (define (get-chunk id cache table lru)
    (let ((i (hashtable-ref table id #F)))
      (if i
        (begin (update-LRU! lru i)
               (vector-ref cache i))
        (let ((c (load-chunk id)))
          (cache-chunk c cache table lru)
          c))))

  (define (get-data-chunk id) (get-chunk id data-access-unit data-assoc-unit data-lru))
  (define (get-inst-chunk id) (get-chunk id inst-access-unit inst-assoc-unit inst-lru))

  ; TODO?: Cache of new chunks ready to be allocated?  Will have to free them
  ; when processor stops.

  ;-----------------------------------------------------------------------------

  (define send*)

  (define (cache:set-storage-comm! sender receiver)
    (set! send* sender))

)
