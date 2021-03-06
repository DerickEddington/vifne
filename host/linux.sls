#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

(library (vifne host linux)
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
    (rnrs base)
    (rnrs io simple)
    (rnrs io ports)
    (vifne posix sysconf))

  (define (number-host-processors) (sysconf _SC_NPROCESSORS_ONLN))

  (define message-queue-library-name "librt.so")

  (define (message-queue-msgsize_max)
    (call-with-input-file "/proc/sys/fs/mqueue/msgsize_max" get-datum))

  ; These values were taken from my Ubuntu 10.10 x86_64 system.

  (define _SC_NPROCESSORS_ONLN  84)

  (define PROT_READ   1)
  (define PROT_WRITE  2)

  (define MAP_SHARED   1)
  (define MAP_FAILED  -1)

  (define O_RDONLY    0)
  (define O_WRONLY    1)
  (define O_RDWR      2)
  (define O_CREAT  #x40)
  (define O_EXCL   #x80)

  (define S_IRUSR  #o400)
  (define S_IWUSR  #o200)

  (define SIGTERM 15)

)
