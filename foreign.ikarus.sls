#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library uses the "foreign" facilities of Ikarus to provide the library
; needed to access procedures and memory that are foreign to Ikarus.  I.e. this
; library provides the means of calling C procedures like mmap and the means to
; access memory, e.g. mmap'ed memory, not managed by the Scheme system.

(library (vifne foreign)
  (export
    foreign-library
    foreign
    string->c-str
    c-str->string
    pointer->integer
    integer->pointer
    pointer?
    NULL
    memcpy
    pointer-ref-u64
    pointer-set-u64!
    pointer-ref-s32)
  (import
    (rnrs base)
    (rnrs io ports)
    (rnrs bytevectors)
    (ikarus foreign))

  (define NULL (integer->pointer 0))

  (define (dlfail who . a) (apply error who (dlerror) a))

  (define handle (or (dlopen) (dlfail 'handle)))

  (define (foreign-library name)
    (or (dlopen name #T #F)  ; #T for lazy, #F for local
        (dlfail 'foreign-library name)))

  (define (foreign-procedure name type lib)
    ((apply make-c-callout type)
     (or (dlsym lib name) (dlfail 'foreign-procedure name))))

  (define-syntax foreign
    (syntax-rules ()
      ((_ (name arg-types ...) ret-type lib)
       (foreign-procedure name '(ret-type (arg-types ...)) lib))
      ((_ (n a ...) r)
       (foreign (n a ...) r handle))))

  (define NT (native-transcoder))

  (define (string->c-str s)
    ; Remember: the returned pointer needs to be freed.
    (let* ((b (string->bytevector s NT))
           (l (bytevector-length b))
           (m (malloc (+ 1 l))))  ; + 1 for terminating '\0'
      (assert (not (zero? (pointer->integer m))))
      (memcpy m 0 b 0 l)
      (pointer-set-c-char! m l 0)
      m))

  (define (c-str->string c)
    (let loop ((a '()) (i 0))
      (let ((x (pointer-ref-c-unsigned-char c i)))
        (if (zero? x)
          (bytevector->string (u8-list->bytevector (reverse a)) NT)
          (loop (cons x a) (+ 1 i))))))

  (define (pointer-ref-u64 p i) (pointer-ref-c-unsigned-long-long p (* 8 i)))
  (define (pointer-set-u64! p i v) (pointer-set-c-long-long! p (* 8 i) v))
  (define (pointer-ref-s32 p i) (pointer-ref-c-signed-int p (* 4 i)))

)
