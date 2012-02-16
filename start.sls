#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

(library (vifne start)
  (export
    start-emulator
    stop-emulator)
  (import
    (rnrs base)
    (vifne posix)
    (vifne storage))

  (define (start-emulator storage-file number-processors)
    (storage-set! (mmap-storage-file storage-file)
                  (file-size storage-file)))

  (define (stop-emulator)
    (apply munmap (storage-get)))

)
