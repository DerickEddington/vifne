#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

(library (vifne posix)
  (export
    mmap
    munmap
    open
    close
    file-size
    getpid
    fork
    sleep
    kill
    malloc
    free
    error/errno
    NULL
    PROT_READ
    PROT_WRITE
    MAP_SHARED
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
    (rnrs control)
    (vifne foreign)
    (vifne host)
    (vifne posix misfits))


  (define strerror-raw (foreign ("strerror" signed-int) pointer))

  (define (strerror e) (c-str->string (strerror-raw e)))

  (define (error/errno who . args)
    (apply error who (strerror (errno)) args))


  (define getpid (foreign ("getpid") signed-int))  ; returns pid_t


  (define fork-raw (foreign ("fork") signed-int))  ; returns pid_t

  (define (fork)
    (let ((r (fork-raw)))
      (when (negative? r) (error/errno 'fork))
      r))


  (define sleep (foreign ("sleep" unsigned-int)  ; unsigned  seconds
                         unsigned-int))  ; returns unsigned


  (define kill-raw (foreign ("kill" signed-int   ; pid_t  pid
                                    signed-int)  ; int  sig
                            signed-int))  ; returns int

  (define (kill pid sig) (unless (zero? (kill-raw pid sig)) (error/errno 'kill pid sig)))


  (define malloc-raw (foreign ("malloc" unsigned-long)  ; size_t  size
                              pointer))  ; returns void*

  (define (malloc size)
    (let ((p (malloc-raw size)))
      (when (and (positive? size) (zero? (pointer->integer p)))
        (error/errno 'malloc size))
      p))

  (define free (foreign ("free" pointer)  ; void*  ptr
                        void))


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


  (define close-raw (foreign ("close" signed-int)  ; int  fd
                             signed-int))  ; returns int

  (define (close fd) (unless (zero? (close-raw fd)) (error/errno 'close fd)))

)
