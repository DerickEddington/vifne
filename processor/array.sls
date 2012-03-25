#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO.  It must only be used after forking a process for a
; processor, after the storage is properly initialized.

(library (vifne processor array)
  (export
    data-array-ref
    inst-array-ref)
  (import
    (rnrs base)
    (rnrs control)
    (vifne config)
    (vifne processor cache)
    (vifne processor exception))

  (define (array-ref handle-id index get-chunk)
    (define depth-field 0)
    (define tree-field  1)
    (let ((h (get-chunk handle-id)))
      (let-values (((depth dp?)   (chunk-ref h depth-field))
                   ((tree-id tp?) (chunk-ref h tree-field)))
        (when (or dp? (not tp?)) (processor-exception 'invalid-array))
        (let loop ((d depth) (i index) (t tree-id))
          (if (zero? d)
            (if (< i chunk-wsz)
              ; Note that the index is assumed to be valid if it's within a
              ; chunk at the specified depth.  It's the user's responsibility to
              ; know if an elements-chunk is not entirely used.
              (chunk-ref (get-chunk t) i)
              ; This bounds check is only done to prevent the processor from
              ; getting stuck in an unrecoverable state.  It does not guarantee
              ; the index is in-bounds (see above).
              (processor-exception 'out-of-bounds))
            (let*-values (((i r) (div-and-mod i (expt chunk-wsz d)))
                          ((t tp?) (chunk-ref (get-chunk t) i)))
              (unless tp? (processor-exception 'out-of-bounds))
              (loop (- d 1) r t)))))))

  (define (data-array-ref id i) (array-ref id i get-data-chunk))
  (define (inst-array-ref id i) (array-ref id i get-inst-chunk))

)
