#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library exports values that might differ across different host platforms.
; Import the (vifne host <name>) library for your host platform.

(library (vifne host)
  (export
    number-host-processors
    message-queue-library-name
    message-queue-msgsize_max
    PROT_READ
    PROT_WRITE
    MAP_SHARED
    MAP_FAILED
    O_RDONLY
    O_WRONLY
    O_RDWR
    O_CREAT
    O_EXCL
    S_IRUSR
    S_IWUSR
    SIGTERM)
  (import
    (vifne host linux)
    #;(vifne host freebsd))
)
