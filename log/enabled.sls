#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library processes the command-line arguments at expand-time, looking for
; the --log option.

(library (vifne log enabled)
  (export
    enabled)
  (import
    (rnrs base)
    (rnrs lists)
    (rnrs io simple)
    (rnrs io ports)
    (vifne util command-line))

  (define-command-line-arguments
    (enabled "--log"
             (open-string-input-port read)
             (lambda (x) (and (list? x) (for-all symbol? x)))
             '()))

)
