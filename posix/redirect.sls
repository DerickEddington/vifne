#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library provides the means to redirect the standard output streams to
; files, for separating the output of multiple processes.  It is intended to be
; used by forked child processes.

(library (vifne posix redirect)
  (export
    redirect-stdouts)
  (import
    (rnrs base)
    (vifne posix)
    (vifne main-pid))

  (define stdout 1)
  (define stderr 2)

  (define (open-new name ext)
    (open (string-append "vifne-" (main-pid-str) "-" name
                         "-" (number->string (getpid)) "." ext)
          (+ O_CREAT O_EXCL O_WRONLY)
          S_IRUSR))

  (define (redirect-stdouts name)
    (let ((out (open-new name "out"))
          (err (open-new name "err")))
      (values (dup2 out stdout)
              (dup2 err stderr))))

)
