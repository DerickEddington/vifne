#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library uses Ikarus to provide the needed POSIX facilities.

(library (vifne posix)
  (export
    getconf
    #| TODO
    fork stuff ...
    pipe stuff ...
    mmap stuff ...|#)
  (import
    (rnrs base)
    (rnrs io ports)
    (only (ikarus) process waitpid wstatus-exit-status)
    #| (only (ikarus ???) ??? fork and pipe stuff I think)
    (vifne foreign) to implement mmap stuff FFI |# )

  (define (getconf . a)
    (define (S p)
      (get-string-all (transcoded-port p (native-transcoder))))
    (let-values (((pid in out err)
                  (apply process "getconf" a)))
      (let* ((w (wstatus-exit-status (waitpid pid)))
             (o (S out))
             (e (S err)))
        (for-each close-port (list in out err))
        (if (zero? w) o (apply error 'getconf e a)))))


)
