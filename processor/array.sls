#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO.  It must only be used after forking a process for a
; processor, after the storage is properly initialized.

(library (vifne processor array)
  (export
    array-ref
    fv fp?)
  (import
    (rnrs base)
    (rnrs control)
    (vifne config)
    (only (vifne storage) fv fp?)
    (vifne processor exception))

  (define (array-ref handle-id index)
    (define depth-field 0)
    (define tree-field  1)
    (let ((h (load-chunk* handle-id)))
      (let ((depth   (vector-ref h depth-field))
            (tree-id (vector-ref h tree-field)))
        (when (or (fp? depth) (not (fp? tree-id))) (processor-exception 'invalid-array))
        (let recur ((d (fv depth)) (i index) (t (fv tree-id)))
          (define (t-ref i)
            (vector-ref (load-chunk* t) i))
          (if (zero? d)
            (if (< i chunk-wsz)
              ; Note that the index is assumed to be valid if it's within a
              ; chunk at the specified depth.  It's the user's responsibility to
              ; know if an elements-chunk is not entirely used.
              (t-ref i)
              ; This bounds check is only done to prevent the processor from
              ; getting stuck in an unrecoverable state.  It does not guarantee
              ; the index is in-bounds (see above).
              (processor-exception 'out-of-bounds))
            (let-values (((i r) (div-and-mod i (expt chunk-wsz d))))
              (let ((t (t-ref i)))
                (unless (fp? t) (processor-exception 'out-of-bounds))
                (recur (- d 1) r (fv t)))))))))

)
