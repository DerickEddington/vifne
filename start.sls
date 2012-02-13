#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO

(library (vifne start)
  (export
    start-emulator)
  (import
    (rnrs base)
    (rnrs io simple) #|TODO: temporary import|#
    (vifne posix))

  (define (start-emulator storage-file number-processors)
    ;TODO
    (begin
      (write `(start-emulator ,storage-file ,number-processors)) (newline)

      (let ((p (mmap-storage-file storage-file)))
        (write `(mapped-storage-file-pointer: ,p)) (newline)
        (munmap p (file-size storage-file)))))

)
