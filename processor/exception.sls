#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

; TODO: How to handle user-caused processor exceptions?

; TODO?: Define and export bindings for the exception types?

(library (vifne processor exception)
  (export
    processor-exception?
    processor-exception
    load-chunk*)
  (import
    (rnrs base)
    (rnrs conditions)
    (rnrs exceptions)
    (only (vifne storage) load-chunk))

  (define-condition-type &processor-exception &serious
    make-processor-exception processor-exception?
    (type processor-exception-type))

  (define (processor-exception type) (raise (make-processor-exception type)))

  (define (load-chunk* id) (or (load-chunk id) (processor-exception 'guarded)))

)
