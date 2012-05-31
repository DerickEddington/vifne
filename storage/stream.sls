#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

; TODO: Comments about how if last element is gotten from a stream, the
; tail-index-field of a stream-chunk might be left with an old inconsistent
; value, but this is alright because the nullness of tail-elements-field is what
; matters.  This design choice is made to minimize the amount of operations, for
; performance.  Should add commented-out expression that resets
; tail-index-field.

(library (vifne storage stream)
  (export
    alloc-stream!
    stream-put!
    stream-get!)
  (import
    (rnrs base)
    (rnrs control)
    (vifne config)
    (vifne storage))

  (define stream-tags (tags guard-tag))
  (define head-tags (tags stream-head-tag guard-tag))
  (define tail-tags (tags stream-tail-tag guard-tag))
  (define elements-tags (tags guard-tag))

  ; Stream chunk fields:
  (define stream-wsz 5)
  (define head-elements-field 0)
  (define head-index-field    1)
  (define tail-elements-field 2)
  (define tail-index-field    3)
  (define resume-field        4)
  ; Head or tail handle chunk fields:
  (define handle-stream-field 0)
  ; Elements chunk fields:
  (define next-elements-field (- chunk-wsz 1))


  (define (alloc-stream!*)
    (let ((stream (alloc-chunk! stream-tags 0))) ; Ref-count incremented in alloc-stream!.
      (and stream
           (let ((s (id->ptr stream)))
             ; (alloc-chunk! clears the pointer flags, so don't need to use
             ; set-field! for the assignments.)
             (set-word! s head-elements-field 0)
             #;(set-word! s head-index-field 0)
             (set-word! s tail-elements-field 0)
             #;(set-word! s tail-index-field 0)
             (set-word! s resume-field 0)
             stream))))

  (define (alloc-stream!)
    (let ((stream (alloc-stream!*))             ; Ref-count incremented below.
          (head (alloc-chunk! head-tags))       ; Ref-count = 1
          (tail (alloc-chunk! tail-tags))       ; Ref-count = 1
          (resume (alloc-stream!*)))            ; Ref-count incremented below.
      (if (and stream head tail resume)
        (let ((s (id->ptr stream)) (h (id->ptr head)) (t (id->ptr tail)))
          (set-field! s resume-field resume #T)
          (set-field! h handle-stream-field stream #T)
          (set-field! t handle-stream-field stream #T)
          (list head tail))
        (begin (for-each (lambda (x) (when x (free-chunk! x)))
                         (list stream head tail resume))
               #F))))


  (define (stream-put!* val ptr? s)
    (define (set-elem! x i)
      (set-field! (id->ptr x) i val ptr?))
    (define (done)
      ; Check if there is a procedure waiting for an element.
      (let-values (((n p?) (ref-field s resume-field)))
        (and p? (let ((x (stream-get!* #F (id->ptr n))))
                  (and (pair? x) (car x))))))
    (let-values (((te te-p?) (ref-field s tail-elements-field))
                 ((i) (ref-word s tail-index-field)))
      (if (and te-p? (< i next-elements-field))
        ; There is already an available slot.
        (begin (set-elem! te i)
               (set-word! s tail-index-field (+ 1 i))
               (done))
        ; Need a new chunk.
        (let ((c (alloc-chunk! elements-tags 0))) ; Ref-count incremented below.
          (if c
            (begin (set-elem! c 0)
                   (set-word! (id->ptr c) next-elements-field 0)
                   (set-field! s tail-elements-field c #T)
                   (set-word! s tail-index-field 1)
                   (if te-p?
                     (set-field! (id->ptr te) next-elements-field c #T)
                     (begin (assert (not (ptr-field? s head-elements-field)))
                            (set-field! s head-elements-field c #T)
                            (set-word! s head-index-field 0)))
                   (done))
            'storage-full)))))

  (define (stream-put! tail-id val ptr?)
    (let ((t (id->ptr tail-id)))
      (if (tagged? t stream-tail-tag)
        (stream-put!* val ptr? (id->ptr (ref-word t handle-stream-field)))
        'not-tail)))


  (define (stream-get!* resume-proc-id s)
    (let-values (((he he-p?) (ref-field s head-elements-field))
                 ((i) (ref-word s head-index-field)))
      (if he-p?
        ; There is an available element.
        (let-values (((val ptr?) (ref-field (id->ptr he) i)))
          (when ptr? (incr-refcount! val)) ; Because it's returned.
          (set-field! (id->ptr he) i 0 #F)
          (let ((i (+ 1 i))
                (te (ref-word s tail-elements-field)))
            (if (and (< i next-elements-field)
                     (or (not (= te he))
                         (< i (ref-word s tail-index-field))))
              (set-word! s head-index-field i)
              ; No more elements in what was the head chunk.  Move the
              ; head to the next chunk.
              (let-values (((n n-p?) (ref-field (id->ptr he) next-elements-field)))
                (if n-p?
                  (begin (assert (not (= te he)))
                         (set-field! s head-elements-field n #T)
                         (set-word! s head-index-field 0))
                  (begin (assert (= te he))
                         (set-field! s head-elements-field 0 #F)
                         #;(set-word! s head-index-field 0)
                         (set-field! s tail-elements-field 0 #F)
                         #;(set-word! s tail-index-field 0))))))
          (list val ptr?))
        ; Stream is empty.  If given, save resume-proc-id for when an element is
        ; added.
        (or (and resume-proc-id
                 (let-values (((n p?) (ref-field s resume-field)))
                   (assert p?)
                   ; stream-put!* will return #F or 'storage-full
                   (stream-put!* resume-proc-id #T (id->ptr n))))
            'stream-empty))))

  (define (stream-get! head-id resume-proc-id)
    (let ((h (id->ptr head-id)))
      (if (tagged? h stream-head-tag)
        (stream-get!* resume-proc-id (id->ptr (ref-word h handle-stream-field)))
        'not-head)))

  ;-----------------------------------------------------------------------------

  ; The chunk size must have enough words for the stream fields.
  (assert (<= stream-wsz chunk-wsz))
  ; The chunk size must be enough for hold an element and the "next" field.
  (assert (< 1 chunk-wsz))

)
