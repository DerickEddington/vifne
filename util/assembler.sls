#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library provides an assembler, that operates outside the emulator, that
; enables constructing data structures, especially instruction segments, in an
; emulator storage file, including the ability to bypass capability-security
; with magic pointers.  Its assembly language is syntactically made of Scheme
; datums ... TODO more.

(library (vifne util assembler)
  (export
    assemble)
  (import
    (rnrs base)
    (vifne util assembler storage))

  (define (assemble inst-list magic?)
    TODO)
)
