#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

(library (vifne util misc)
  (export
    list-extend)
  (import
    (rnrs base)
    (rnrs control))

  (define (list-extend l size x)
    (append l (do ((i (length l) (+ 1 i))
                   (a '() (cons x a)))
                  ((<= size i) a))))

)
