#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

(library (vifne posix)
  (export
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
    #| TODO: fork stuff ...|#)
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


  (define close-raw (foreign ("close" signed-int) signed-int))

  (define (close fd) (unless (zero? (close-raw fd)) (error/errno 'close fd)))


  (define (open-storage-file file) (open file O_RDWR))

  (define (mmap-storage-file file)
    (let* ((fd (open-storage-file file))
           (p (mmap NULL (file-size file) (+ PROT_READ PROT_WRITE) MAP_SHARED fd 0)))
      (close fd)
      p))

  ;-----------------------------------------------------------------------------

  ; TODO: fork stuff

)
