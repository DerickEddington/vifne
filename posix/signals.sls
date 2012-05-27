#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library provides the means to block signals.

(library (vifne posix signals)
  (export
    block-signals)
  (import
    (rnrs base)
    (vifne posix))

  (define (block-signals)
    ; All the Posix signals except KILL and those that indicate a low-level
    ; problem.
    (sigprocmask 'add '(SIGABRT SIGALRM SIGBUS SIGHUP SIGINT SIGQUIT
                        SIGTERM SIGUSR1 SIGUSR2 SIGPOLL SIGPROF
                        SIGSYS SIGTRAP SIGVTALRM SIGXCPU SIGXFSZ)))

)
