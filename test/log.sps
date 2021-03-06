#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

(import (rnrs)
        (vifne log)
        (vifne main-pid)
        (vifne posix))

(main-pid-set! (getpid))

(define-logger log/foo foo (string-append "la" "la") "haha")
(define-logger log/bar bar)

(log/foo (+ 1 2))
(log/bar (/ 7 17))
(log/foo "second")
(log/bar 'nd2)
