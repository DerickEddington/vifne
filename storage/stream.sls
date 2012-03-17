#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

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
  (define notify-field        4)
  ; Head or tail handle chunk fields:
  (define handle-stream-field 0)
  ; Elements chunk fields:
  (define next-elements-field (- chunk-wsz 1))


  (define (alloc-stream!)
    (let ((stream (alloc-chunk! stream-tags 0)) ; Ref-count incremented below.
          (head (alloc-chunk! head-tags))       ; Ref-count = 1
          (tail (alloc-chunk! tail-tags)))      ; Ref-count = 1
      (if (and stream head tail)
        (let ((s (id->ptr stream)) (h (id->ptr head)) (t (id->ptr tail)))
          ; (alloc-chunk! clears the pointer flags, so don't need to use
          ; set-field! for all the assignments.)
          (set-word! s head-elements-field 0)
          #;(set-word! s head-index-field 0)
          (set-word! s tail-elements-field 0)
          #;(set-word! s tail-index-field 0)
          (set-word! s notify-field 0)
          (set-field! h handle-stream-field stream #T)
          (set-field! t handle-stream-field stream #T)
          (list head tail))
        (begin (for-each (lambda (x) (when x (free-chunk! x)))
                         (list stream head tail))
               #F))))


  (define (stream-put! tail-id val ptr?)

    (define (set-elem! x i) (set-field! (id->ptr x) i val ptr?))

    (let ((t (id->ptr tail-id)))
      (if (tagged? t stream-tail-tag)
        (let ((s (id->ptr (ref-word t handle-stream-field))))

          (define (done)
            ; Check if there is a procedure waiting for an element.
            (let ((n (ref-word s notify-field)))
              (and (positive? n)
                   (begin (incr-refcount! n) ; Because it's returned.
                          ; TODO?: Should it be cleared?  Might be useful if not.
                          (set-field! s notify-field 0 #F)
                          n))))

          (let ((te (ref-word s tail-elements-field))
                (i (ref-word s tail-index-field)))
            (if (and (positive? te) (< i next-elements-field))
              ; There is already an available slot.
              (begin (set-elem! te i)
                     (set-word! s tail-index-field (+ 1 i))
                     (done))
              ; Need a new chunk.
              (let ((c (alloc-chunk! elements-tags 0))) ; Ref-count incremented below.
                (if c
                  (begin (set-elem! c 0)
                         (set-word! c next-elements-field 0) ; (set-field! unnecessary.)
                         (set-field! s tail-elements-field c #T)
                         (set-word! s tail-index-field 1)
                         (if (positive? te)
                           (set-field! te next-elements-field c #T)
                           (begin (assert (zero? (ref-word s head-elements-field)))
                                  (set-field! s head-elements-field c #T)
                                  (set-word! s head-index-field 0)))
                         (done))
                  'storage-full)))))
        'not-tail)))


  (define (stream-get! head-id notify-proc-id)
    (let ((h (id->ptr head-id)))
      (if (tagged? h stream-head-tag)
        (let ((s (id->ptr (ref-word h handle-stream-field))))
          (let ((he (ref-word s head-elements-field))
                (i (ref-word s head-index-field)))
            (if (positive? he)
              ; There is an available element.
              (let-values (((val ptr?) (ref-field he i)))
                (when ptr? (incr-refcount! val)) ; Because it's returned.
                (set-field! he i 0 #F)
                (let ((i (+ 1 i))
                      (te (ref-word s tail-elements-field)))
                  (if (and (< i next-elements-field)
                           (or (not (= te he))
                               (< i (ref-word s tail-index-field))))
                    (set-word! s head-index-field i)
                    ; No more elements in what was the head chunk.  Move the
                    ; head to the next chunk.
                    (let ((n (ref-word he next-elements-field)))
                      (if (positive? n)
                        (begin (not (assert (= te he)))
                               (set-field! s head-elements-field n #T)
                               (set-word! s head-index-field 0))
                        (begin (assert (= te he))
                               (set-field! s head-elements-field 0 #F)
                               #;(set-word! s head-index-field 0)
                               (set-field! s tail-elements-field 0 #F)
                               #;(set-word! s tail-index-field 0))))))
                (list val ptr?))
              ; Stream is empty.  Save notify-proc-id for when an element is added.
              (begin (when notify-proc-id
                       (assert (zero? (ref-word s notify-field)))
                       (set-field! s notify-field notify-proc-id #T))
                     'stream-empty))))
        'not-head)))

  ;-----------------------------------------------------------------------------

  ; The chunk size must have enough words for the stream fields.
  (assert (<= stream-wsz chunk-wsz))
  ; The chunk size must be enough for hold an element and the "next" field.
  (assert (< 1 chunk-wsz))

)
