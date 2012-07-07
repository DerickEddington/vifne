#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

(library (vifne test call return-linkage)
  (export
    return-linkage-test)
  (import
    (vifne util assembler high-lang)
    (vifne test lang-util)
    (rnrs base))

 (define (return-linkage-test control-transfer prompt-word)

   (define (how-many?)
     (prompt/nat (string-append prompt-word " how many times? ")))

   (define arg0 r0)
   (define arg1 r1)
   (define arg2 r2)

   (define (loop)
     (define one arg2)
     (assemble
      (jump-zero arg0 'done)
      (sub arg0 arg0 one)
      (control-transfer arg1)
      (label 'done)
      (return)))

   (set-immediate arg0 (how-many?))
   (set-multiple-immediates (group arg1))
   (loop)
   (set-immediate arg2 1)
   (call arg1 rFF rFF) ; (Don't actually care about saving any registers.)
   (test arg0 0))

)
