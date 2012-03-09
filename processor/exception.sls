#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library is separate from (vifne processor) to avoid circular imports with
; (vifne processor cache) and (vifne processor).

(library (vifne processor exception)
  (export
    (rename (raise-processor-exception processor-exception))
    processor-exception-type)
  (import
    (rnrs base)
    (rnrs records syntactic)
    (rnrs exceptions))

  (define-record-type processor-exception (fields type))

  (define (raise-processor-exception type) (raise (make-processor-exception type)))

  ; TODO?: Define and export bindings for the exception types?

)
