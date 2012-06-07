#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library TODO.  The operations must only be called after the emulator is
; properly initialized, but the operation-info things may be used before that
; (e.g. by the assembler).

(library (vifne processor operations)
  (export
    do-operation
    operation-code-bitsize
    get-operation-info
    operation-info-code
    operation-info-operand-bitsizes
    operation-info-operand-validators
    operations:set-storage-comm!)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs lists)
    (rnrs records syntactic)
    (rnrs arithmetic bitwise)
    (rnrs hashtables)
    (vifne config)
    (only (vifne storage) store-chunk! f fv fp?)
    (vifne processor registers)
    (vifne processor array)
    (vifne processor exception)
    (vifne log)
    (only (vifne posix) getpid))

  (define operation-code-bitsize 16)

  (define (do-operation inst-word)
    (let ((opcode (bitwise-and #xFFFF inst-word)))
      (unless (< opcode (vector-length operations-table))
        (processor-exception 'invalid-instruction))
      ((vector-ref operations-table opcode) inst-word)))


  (define (make-operation-proc bitsizes proc)
    (assert (for-all exact-positive-integer? bitsizes))
    (assert (<= (apply + bitsizes) (- 64 operation-code-bitsize)))
    (lambda (inst-word)
      (apply proc (let loop ((operands '())
                             (curr operation-code-bitsize)
                             (bsz bitsizes))
                    (if (null? bsz)
                      (reverse operands)
                      (let ((next (+ (car bsz) curr)))
                        (loop (cons (bitwise-bit-field inst-word curr next)
                                    operands)
                              next
                              (cdr bsz))))))))


  ; Operation information used for assembly language.
  (define-record-type operation-info
    (fields symbol code operand-bitsizes operand-validators))

  (define (get-operation-info sym) (hashtable-ref operations-infos sym #F))


  (define-syntax define-operations
    (syntax-rules ()
      ((_ (proc-table info-table)
          ((op-sym (operand bitsize pred) ...) . body)
          ...)
       (begin (define proc-table
                (vector (make-operation-proc '(bitsize ...)
                          (lambda (operand ...)
                            (debug `(op-sym (operand ,operand) ...))
                            (let () . body)))
                        ...))
              (define info-table
                (let ((ht (make-eq-hashtable (length '(op-sym ...)))))
                  (do ((i 0 (+ 1 i))
                       (s '(op-sym ...)               (cdr s))
                       (b '((bitsize ...) ...)        (cdr b))
                       (p (list (list pred ...) ...)  (cdr p)))
                      ((null? s))
                    (hashtable-set! ht (car s)
                      (make-operation-info (car s) i (car b) (car p))))
                  ht))))))


  (define-operations (operations-table operations-infos)

    ((ignore) #F)


    ((chunk-create (dest    16 register-code?)
                   (grp-sel 16 group-mask?)
                   (src-grp 16 register-code?))
     ; TODO?: If in the future there's a cache of chunks for allocating, this
     ; will change to use it, instead of communicating with the storage
     ; controller here.
     (send* '(allocate))
     (let ((x (receive*)))
       (when (symbol? x) (processor-exception x))
       (let ((id (cadr x)))
         ; Set the fields of the chunk according to the register group
         ; selection.  No other processors have access to the chunk, so it's
         ; safe to directly write the mmap'ed memory here.
         (store-chunk! id
           (map (lambda (r) (cond (r (when (rp? r) (send* `(increment ,(rv r))))
                                     (rf r))
                                  (else (f 0 #F))))
                (group-select src-grp grp-sel)))
         ; Set the specified register to point to the chunk, but don't increment
         ; the reference count because the allocation already set it to 1.
         (r-set! dest (f id #T)))))


    ((chunk-get (dest-grp 16 register-code?)
                (grp-sel  16 group-mask?)
                (src      16 register-code?))
     (unless (rp? src) (processor-exception 'not-pointer))
     (for-each (lambda (r f) (when r (r-set! r f #T)))
               (group-select dest-grp grp-sel)
               (load-chunk* (rv src))))


    ((set-multiple-immediates (dest-grp 16 register-code?)
                              (grp-sel  16 group-mask?))
     (let ((i (fold-left (lambda (i r)
                           (r-set! r (array-ref (srv IS) i) #T)
                           (+ 1 i))
                         (srv II)
                         (filter values (group-select dest-grp grp-sel)))))
       (sr-set! II (f i #F))))


    ((set-immediate (dest 16 register-code?)
                    (v    32 signed-32bit?))
     (r-set! dest
             ; v is always non-negative when extracted.
             (f (if (bitwise-bit-set? v 31)
                  (bitwise-ior #xFFFFFFFF00000000 v)
                  v)
                #F)))


    ((copy (dest 16 register-code?)
           (src  16 register-code?))
     (r-set! dest (rf src) #T))


    ((ior (dest 16 register-code?)
          (src1 16 register-code?)
          (src2 16 register-code?))
     (arith bitwise-ior dest src1 src2))


    ((sub (dest 16 register-code?)
          (src1 16 register-code?)
          (src2 16 register-code?))
     (arith (lambda (a b) (let ((x (- a b))) (if (negative? x) (+ (expt 2 64) x) x)))
            dest src1 src2))


    ((jump-zero (test  16 register-code?)
                (index 32 unsigned-32bit?))
     (jump zero? test index))


    ((jump-positive (test  16 register-code?)
                    (index 32 unsigned-32bit?))
     (jump positive? test index))


    ((jump-negative (test  16 register-code?)
                    (index 32 unsigned-32bit?))
     (jump negative? test index))


    ((goto (seg 16 register-code?))
     (unless (rp? seg) (processor-exception 'not-pointer))
     (sr-set! IS (rf seg) #T)
     (sr-set! II (f 0 #F)))

    )


  (define (group-select reg sel)
    (let ((group (bitwise-and #xFFF0 reg)))
      (do ((i 0 (+ 1 i))
           (r '() (cons (and (bitwise-bit-set? sel i) (+ group i)) r)))
          ((= chunk-wsz i) r))))


  (define (arith proc dest src1 src2)
    (when (or (rp? src1) (rp? src2)) (processor-exception 'pointer))
    (r-set! dest (f (proc (rv src1) (rv src2)) #F)))


  (define (jump pred test index)
    (when (rp? test) (processor-exception 'pointer))
    (when (pred (rv test)) (sr-set! II (f index #F))))

  ;-----------------------------------------------------------------------------

  (define send*)
  (define receive*)

  (define (operations:set-storage-comm! sender receiver)
    (set! send* sender)
    (set! receive* receiver))

  ;-----------------------------------------------------------------------------

  (define-logger debug processor-operations (number->string (getpid)))

  ;-----------------------------------------------------------------------------

  (assert (<= (vector-length operations-table) limit-16bit))

)
