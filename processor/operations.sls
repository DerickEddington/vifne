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
    (vifne processor registers)
    (vifne processor cache)
    (vifne processor array)
    (vifne processor exception))

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
                          (lambda (operand ...) . body))
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
     (send* '(allocate 1))
     (let ((x (receive*)))
       (when (symbol? x) (processor-exception x))
       (let* ((id (cadr x))
              (c (new-chunk id)))
         (let-values (((r i) (group-select src-grp grp-sel)))
           ; Set the fields of the chunk according to the register group
           ; selection, and collect any pointers set in the fields.
           (let ((incr (fold-left (lambda (a r i)
                                    (chunk-set! c i (rv r) (rp? r))
                                    (if (rp? r) (cons (rv r) a) a))
                                  '() r i)))
             ; Increment the reference counts of any chunks now referenced by
             ; the fields of the chunk.
             (unless (null? incr) (send* `(increment . ,incr)))))
         ; Store the chunk in the shared storage at this point in case another
         ; processor needs to access the chunk.
         (store-chunk! c)
         ; Set the specified register to point to the chunk, but don't increment
         ; the reference count because the allocation already set it to 1.
         (r-set! dest id #T #F))))


    ((chunk-get (dest-grp 16 register-code?)
                (grp-sel  16 group-mask?)
                (src      16 register-code?))
     (unless (rp? src) (processor-exception 'not-pointer))
     (let ((c (get-data-chunk (rv src))))
       (let-values (((r i) (group-select dest-grp grp-sel)))
         (for-each (lambda (r i)
                     (let-values (((v p?) (chunk-ref c i)))
                       (r-set! r v p? #T)))
                   r i))))


    ((set-multiple-immediates (dest-grp 16 register-code?)
                              (grp-sel  16 group-mask?))
     (let-values (((r _) (group-select dest-grp grp-sel)))
       (register-value-set! (sr II)
         (fold-left (lambda (i r)
                      (let ((i (+ 1 i)))
                        (let-values (((v p?) (inst-array-ref (srv IS) i)))
                          (r-set! r v p? #T))
                        i))
                    (srv II) r))))


    ((set-immediate (dest 16 register-code?)
                    (v    32 signed-32bit?))
     (r-set! dest
             ; v is always non-negative when extracted.
             (if (bitwise-bit-set? v 31)
               (bitwise-ior #xFFFFFFFF00000000 v)
               v)
             #F #F))


    ((copy (dest 16 register-code?)
           (src  16 register-code?))
     (r-set! dest (rv src) (rp? src) #T))


    ((get-special (dest 16 register-code?)
                  (src  16 special-register-code?))
     (r-set! dest (srv src) (srp? src) #T))


    ((set-special (dest 16 special-register-code?)
                  (src  16 register-code?))
     (sr-copy! dest src))


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


    ((goto (seg 16 register-code?)
           (i   16 register-code?))
     (unless (rp? seg) (processor-exception 'not-pointer))
     (when (rp? i) (processor-exception 'pointer))
     (set-register! (sr IS) (rv seg) #T #T)
     (register-value-set! (sr II) (rv i)))

    )


  (define (group-select reg sel)
    (let ((group (bitwise-and #xFFF0 reg)))
      (let loop ((i (- chunk-wsz 1)) (r '()) (n '()))
        (if (negative? i)
          (values r n)
          (if (bitwise-bit-set? sel i)
            (loop (- i 1) (cons (+ group i) r) (cons i n))
            (loop (- i 1) r n))))))


  (define (arith proc dest src1 src2)
    (when (or (rp? src1) (rp? src2)) (processor-exception 'pointer))
    (r-set! dest (proc (rv src1) (rv src2)) #F #F))


  (define (jump pred test index)
    (when (rp? test) (processor-exception 'pointer))
    (when (pred (rv test)) (register-value-set! (sr II) index)))

  ;-----------------------------------------------------------------------------

  (define send*)
  (define receive*)

  (define (operations:set-storage-comm! sender receiver)
    (set! send* sender)
    (set! receive* receiver))

  ;-----------------------------------------------------------------------------

  (assert (<= (vector-length operations-table) limit-16bit))

)
