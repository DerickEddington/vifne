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
           (begin (set-field! stream head-elements-field (f 0 #F))
                  #;(set-field! stream head-index-field (f 0 #F))
                  (set-field! stream tail-elements-field (f 0 #F))
                  #;(set-field! stream tail-index-field (f 0 #F))
                  (set-field! stream resume-field (f 0 #F))
                  stream))))

  (define (alloc-stream!)
    (let ((stream (alloc-stream!*))             ; Ref-count incremented below.
          (head (alloc-chunk! head-tags))       ; Ref-count = 1
          (tail (alloc-chunk! tail-tags))       ; Ref-count = 1
          (resume (alloc-stream!*)))            ; Ref-count incremented below.
      (if (and stream head tail resume)
        (begin (set-field! stream resume-field (f resume #T))
               (set-field! head handle-stream-field (f stream #T))
               (set-field! tail handle-stream-field (f stream #T))
               (list head tail))
        (begin (for-each (lambda (x) (when x (free-chunk! x)))
                         (list stream head tail resume))
               #F))))


  (define (stream-put!* x s)
    (define (set-elem! c i) (set-field! c i x))
    (define (done)
      ; Check if there is a procedure waiting for an element.
      (let ((r (ref-field s resume-field)))
        (and (fp? r) (let ((r (stream-get!* #F (fv r))))
                       (and (f? r) (fv r))))))
    (let ((te (ref-field s tail-elements-field))
          (i (fv (ref-field s tail-index-field))))
      (if (and (fp? te) (< i next-elements-field))
        ; There is already an available slot.
        (begin (set-elem! (fv te) i)
               (set-field! s tail-index-field (f (+ 1 i) #F))
               (done))
        ; Need a new chunk.
        (let ((c (alloc-chunk! elements-tags 0))) ; Ref-count incremented below.
          (if c
            (begin (set-elem! c 0)
                   (set-field! c next-elements-field (f 0 #F))
                   (set-field! s tail-elements-field (f c #T))
                   (set-field! s tail-index-field (f 1 #F))
                   (if (fp? te)
                     (set-field! (fv te) next-elements-field (f c #T))
                     (begin (assert (not (fp? (ref-field s head-elements-field))))
                            (set-field! s head-elements-field (f c #T))
                            (set-field! s head-index-field (f 0 #F))))
                   (done))
            'storage-full)))))

  (define (stream-put! tail-id x)
    (if (tagged? tail-id stream-tail-tag)
      (stream-put!* x (fv (ref-field tail-id handle-stream-field)))
      'not-tail))


  (define (stream-get!* resume-proc-id s)
    (let ((he (ref-field s head-elements-field))
          (i (fv (ref-field s head-index-field))))
      (if (fp? he)
        ; There is an available element.
        (let ((x (ref-field (fv he) i)))
          (when (fp? x) (incr-refcount! (fv x))) ; Because it's returned.
          (set-field! (fv he) i (f 0 #F))
          (let ((i (+ 1 i))
                (te (ref-field s tail-elements-field)))
            (if (and (< i next-elements-field)
                     (or (not (f= te he))
                         (< i (fv (ref-field s tail-index-field)))))
              (set-field! s head-index-field (f i #F))
              ; No more elements in what was the head chunk.  Move the
              ; head to the next chunk.
              (let ((n (ref-field (fv he) next-elements-field)))
                (if (fp? n)
                  (begin (assert (not (f= te he)))
                         (set-field! s head-elements-field n)
                         (set-field! s head-index-field (f 0 #F)))
                  (begin (assert (f= te he))
                         (set-field! s head-elements-field (f 0 #F))
                         #;(set-field! s head-index-field (f 0 #F))
                         (set-field! s tail-elements-field (f 0 #F))
                         #;(set-field! s tail-index-field (f 0 #F)))))))
          x)
        ; Stream is empty.  If given, save resume-proc-id for when an element is
        ; added.
        (or (and resume-proc-id
                 (let ((r (ref-field s resume-field)))
                   (assert (fp? r))
                   ; stream-put!* will return #F or 'storage-full
                   (stream-put!* (f resume-proc-id #T) (fv r))))
            'stream-empty))))

  (define (stream-get! head-id resume-proc-id)
    (if (tagged? head-id stream-head-tag)
      (stream-get!* resume-proc-id (fv (ref-field head-id handle-stream-field)))
      'not-head))

  ;-----------------------------------------------------------------------------

  ; The chunk size must have enough words for the stream fields.
  (assert (<= stream-wsz chunk-wsz))
  ; The chunk size must be enough for hold an element and the "next" field.
  (assert (< 1 chunk-wsz))

)
