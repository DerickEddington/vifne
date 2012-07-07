#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

(library (vifne storage stream)
  (export
    alloc-stream!
    stream-put!
    stream-get!
    clear-waiters!)
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
  (define waiters-field       4)
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
                  (set-field! stream waiters-field (f 0 #F))
                  stream))))

  (define (alloc-stream!)
    (let ((stream (alloc-stream!*))             ; Ref-count incremented below.
          (head (alloc-chunk! head-tags))       ; Ref-count = 1
          (tail (alloc-chunk! tail-tags))       ; Ref-count = 1
          (waiters (alloc-stream!*)))           ; Ref-count incremented below.
      (if (and stream head tail waiters)
        (begin (set-field! stream waiters-field (f waiters #T))
               (set-field! head handle-stream-field (f stream #T))
               (set-field! tail handle-stream-field (f stream #T))
               (list head tail))
        (begin (for-each (lambda (x) (when x (free-chunk! x)))
                         (list stream head tail waiters))
               #F))))


  (define (stream-put!* s x incr? if-waiter?)
    (define (set-elem! c i)
      (set-field! c i x)
      ; If x is a pointer, set-field! incremented the reference count of the
      ; chunk.  If that's undesired, adjust it.
      (when (and (fp? x) (not incr?)) (decr-refcount! (fv x))))
    (define (waiter)
      (let ((w (ref-field s waiters-field)))
        (and (fp? w) (let ((w (stream-get!* (fv w) #F #F)))
                       (and (f? w) w)))))
    (let ((w (waiter)))
      (if (or (not w) if-waiter?)
        ; Put the element in the stream.
        (let ((te (ref-field s tail-elements-field))
              (i (fv (ref-field s tail-index-field))))
          (if (and (fp? te) (< i next-elements-field))
            ; There is already an available slot.
            (begin (set-elem! (fv te) i)
                   (set-field! s tail-index-field (f (+ 1 i) #F))
                   w)
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
                       w)
                'storage-full))))
        w)))

  (define (stream-put! tail-id x incr? if-waiter?)
    (if (tagged? tail-id stream-tail-tag)
      (stream-put!* (fv (ref-field tail-id handle-stream-field))
                    x incr? if-waiter?)
      'not-tail))


  (define (stream-get!* s waiter incr-waiter?)
    (let ((he (ref-field s head-elements-field))
          (i (fv (ref-field s head-index-field))))
      (if (fp? he)
        ; There is an available element.
        (let ((x (ref-field (fv he) i)))
          (when (fp? x)
            ; Increment the reference count of the chunk pointed to by the
            ; gotten element, before losing the reference in the head-elements
            ; chunk, in case this is the only reference.
            (incr-refcount! (fv x)))
          ; Clear the field where the element was, to not hold unneeded
          ; references, to ensure chunks can be freed as soon as possible.
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
        ; Stream is empty.  If given, save waiter for when an element is added.
        (or (and waiter
                 (let ((w (ref-field s waiters-field)))
                   (assert (fp? w))
                   ; stream-put!* will return #F (success) or 'storage-full
                   (stream-put!* (fv w) waiter incr-waiter? 'irrelevant)))
            'stream-empty))))

  (define (stream-get! head-id waiter incr-waiter?)
    (if (tagged? head-id stream-head-tag)
      (stream-get!* (fv (ref-field head-id handle-stream-field))
                    waiter incr-waiter?)
      'not-head))


  (define (clear-waiters! handle-id)
    (assert (or (tagged? handle-id stream-head-tag)
                (tagged? handle-id stream-tail-tag)))
    (let* ((s (fv (ref-field handle-id handle-stream-field)))
           (w (ref-field s waiters-field)))
      (assert (fp? w))
      (do () ((not (f? (stream-get!* (fv w) #F #F)))))))

  ;-----------------------------------------------------------------------------

  ; The chunk size must have enough words for the stream fields.
  (assert (<= stream-wsz chunk-wsz))
  ; The chunk size must be enough for hold an element and the "next" field.
  (assert (< 1 chunk-wsz))

)
