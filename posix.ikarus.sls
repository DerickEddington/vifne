#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library uses Ikarus to provide the needed POSIX facilities.

(library (vifne posix)
  (export
    getconf
    mmap-storage-file
    munmap
    file-size
    malloc
    free
    error/errno
    O_RDONLY
    O_WRONLY
    O_CREAT
    O_EXCL
    S_IRUSR
    S_IWUSR
    #| TODO
    fork stuff ...|#)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs io ports)
    (only (ikarus) process waitpid wstatus-exit-status file-size)
    (only (ikarus foreign) malloc free errno)
    (vifne foreign)
    #| (only (ikarus ???) ??? fork stuff I think) |# )


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

  ;-----------------------------------------------------------------------------

  ; TODO: These constants' values are probably not universal across all systems.
  (define PROT_READ 1)
  (define PROT_WRITE 2)
  (define MAP_SHARED 1)
  (define MAP_FAILED -1)
  (define O_RDONLY 0)
  (define O_WRONLY 1)
  (define O_RDWR 2)
  (define O_CREAT #x40)
  (define O_EXCL #x80)
  (define S_IRUSR #o400)
  (define S_IWUSR #o200)


  (define strerror-raw (foreign ("strerror" signed-int) pointer))

  (define (strerror e) (c-str->string (strerror-raw e)))

  (define (error/errno who . args)
    (apply error who (strerror (errno)) args))


  (define mmap-raw (foreign ("mmap" pointer          ; void*   addr
                                    unsigned-long    ; size_t  length
                                    signed-int       ; int     prot
                                    signed-int       ; int     flags
                                    signed-int       ; int     fd
                                    signed-long)     ; off_t   offset
                            pointer))  ; returns void*

  (define (mmap . args)
    (let ((p (apply mmap-raw args)))
      (when (= MAP_FAILED (pointer->integer p)) (apply error/errno 'mmap args))
      p))


  (define munmap-raw (foreign ("munmap" pointer         ; void*   addr
                                        unsigned-long)  ; size_t  len
                              signed-int))  ; returns int

  (define (munmap . args)
    (unless (zero? (apply munmap-raw args))
      (apply error/errno 'munmap args)))


  (define open-raw (foreign ("open" pointer           ; char*   pathname
                                    signed-int        ; int     flags
                                    unsigned-int)     ; mode_t  mode
                            signed-int))  ; returns int

  (define open
    (case-lambda
      ((pathname flags mode)
       (let* ((s (string->c-str pathname))
              (fd (open-raw s flags mode)))
         (free s)
         (when (negative? fd) (error/errno 'open pathname flags mode))
         fd))
      ((pathname flags)
       (open pathname flags 0))))


  (define close-raw (foreign ("close" signed-int) signed-int))

  (define (close fd) (unless (zero? (close-raw fd)) (error/errno 'close fd)))


  (define (open-storage-file file) (open file O_RDWR))

  (define (mmap-storage-file file)
    (let* ((fd (open-storage-file file))
           (p (mmap (integer->pointer 0) (file-size file)
                    (+ PROT_READ PROT_WRITE)
                    MAP_SHARED fd 0)))
      (close fd)
      p))

  ;-----------------------------------------------------------------------------

  ; TODO: fork stuff

)
