#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library defines configurable values that normally should not be changed
; but can be if you know what you're doing.

(library (vifne config)
  (export
    chunk-size
    id-size
    default-storage-file
    number-host-processors)
  (import
    (rnrs base)
    (only (vifne posix) getconf))

  (define chunk-size 128)  ; 1024 bits
  (define id-size 8)       ; 64 bits

  (define default-storage-file "shared-chunk-storage")

  (define (number-host-processors)
    (let* ((x (getconf "_NPROCESSORS_ONLN")) (l (string-length x)))
      (assert (and (positive? l) (char=? #\newline (string-ref x (- l 1)))))
      (let ((n (string->number (substring x 0 (- l 1)))))
        (assert (and (integer? n) (exact? n) (not (negative? n))))
        n)))

)
