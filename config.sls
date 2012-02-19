#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library defines configurable values that normally should not be changed
; but can be if you know what you're doing.

(library (vifne config)
  (export
    chunk-size
    id-size
    default-storage-file)
  (import
    (rnrs base))

  (define chunk-size 128)  ; 1024 bits
  (define id-size 8)       ; 64 bits

  (define default-storage-file "shared-chunk-storage")

)
