#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library provides Posix things that cannot be properly implemented using
; the foreign library because C is archaic.

(library (vifne posix misfits)
  (export
    errno
    file-size)
  (import
    (only (ikarus) file-size)
    (only (ikarus foreign) errno))
)
