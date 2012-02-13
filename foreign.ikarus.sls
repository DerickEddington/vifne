#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library uses the "foreign" facilities of Ikarus to provide the library
; needed to access procedures and memory that are foreign to Ikarus.  I.e. this
; library provides the means of calling C procedures like mmap and the means to
; access memory, e.g. mmap'ed memory, not managed by the Scheme system.

(library (vifne foreign)
  (export
    TODO)
  (import
    (rnrs ???)
    (ikarus foreign))


)
