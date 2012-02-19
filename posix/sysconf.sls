#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library is separate from (vifne posix) to avoid circular imports with
; (vifne host linux) and (vifne posix).

(library (vifne posix sysconf)
  (export
    sysconf)
  (import
    (rnrs base)
    (rnrs control)
    (vifne foreign))

  (define sysconf-raw (foreign ("sysconf" signed-int)  ; int  name
                               signed-long))  ; returns long

  (define (sysconf code)
    (let ((r (sysconf-raw code)))
      (when (= -1 r) (error 'sysconf "code invalid or variable has no limit" code))
      r))

)
