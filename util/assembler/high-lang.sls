#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library provides the bindings that the high-level assembly language is
; based on.  TODO: more about how it works.  TODO: Explain why it's designed as
; it is... eval barriers, multiple vs. single instantiation of libraries,
; supporting as much general use of Scheme including bindings control,
; multi-threading unsafety, etc.

(library (vifne util assembler high-lang)
  (export
    eval/accum!

     r0  r1  r2  r3  r4  r5  r6  r7  r8  r9  rA  rB  rC  rD  rE  rF
    r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r1A r1B r1C r1D r1E r1F
    r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r2A r2B r2C r2D r2E r2F
    r30 r31 r32 r33 r34 r35 r36 r37 r38 r39 r3A r3B r3C r3D r3E r3F
    r40 r41 r42 r43 r44 r45 r46 r47 r48 r49 r4A r4B r4C r4D r4E r4F
    r50 r51 r52 r53 r54 r55 r56 r57 r58 r59 r5A r5B r5C r5D r5E r5F
    r60 r61 r62 r63 r64 r65 r66 r67 r68 r69 r6A r6B r6C r6D r6E r6F
    r70 r71 r72 r73 r74 r75 r76 r77 r78 r79 r7A r7B r7C r7D r7E r7F
    r80 r81 r82 r83 r84 r85 r86 r87 r88 r89 r8A r8B r8C r8D r8E r8F
    r90 r91 r92 r93 r94 r95 r96 r97 r98 r99 r9A r9B r9C r9D r9E r9F
    rA0 rA1 rA2 rA3 rA4 rA5 rA6 rA7 rA8 rA9 rAA rAB rAC rAD rAE rAF
    rB0 rB1 rB2 rB3 rB4 rB5 rB6 rB7 rB8 rB9 rBA rBB rBC rBD rBE rBF
    rC0 rC1 rC2 rC3 rC4 rC5 rC6 rC7 rC8 rC9 rCA rCB rCC rCD rCE rCF
    rD0 rD1 rD2 rD3 rD4 rD5 rD6 rD7 rD8 rD9 rDA rDB rDC rDD rDE rDF
    rE0 rE1 rE2 rE3 rE4 rE5 rE6 rE7 rE8 rE9 rEA rEB rEC rED rEE rEF
    rF0 rF1 rF2 rF3 rF4 rF5 rF6 rF7 rF8 rF9 rFA rFB rFC rFD rFE rFF

    group

    data
    pointer
    chunk
    label
    assemble

    ignore
    chunk-create
    chunk-get
    set-multiple-immediates
    set-immediate
    copy
    ior
    sub
    jump
    jump-zero
    jump-positive
    jump-negative
    goto)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs lists)
    (rnrs arithmetic bitwise)
    (rnrs records syntactic)
    (vifne config)
    (only (vifne storage) valid-id?)
    (vifne util misc))

  (define (or? . preds) (lambda (x) (exists (lambda (p) (p x)) preds)))


  ; A stack of accumulators is used so that accumulation can be used
  ; recursively.
  (define accum-stack '())

  (define (start-accum!)
    (set! accum-stack (cons '() accum-stack))
    #F)

  (define (accum! x)
    (set! accum-stack (cons (cons x (car accum-stack)) (cdr accum-stack))))

  (define (end-accum!)
    (let ((a (car accum-stack)))
      (set! accum-stack (cdr accum-stack))
      (let ((a (reverse a)))
        ; If the only thing accumulated is a <chunk> form, take it out of the
        ; list, so that it is a <source> that means to assemble the graph of
        ; chunks specified by the <chunk>.  Otherwise, the accumulated forms are
        ; a <source> that means to assemble a <segment> array-tree.  (This makes
        ; it impossible to specify a <segment> array-tree with a <chunk> as the
        ; only element, but this shouldn't be a problem in practice because such
        ; a <segment> is an invalid instruction sequence that shouldn't be
        ; created.  If it really is desired to create such an instruction
        ; sequence, it can be done by manually specifying such an
        ; instruction-segment array-tree as a <chunk> graph.)
        (if (and (= 1 (length a)) (pair? (car a))
                 (eq? 'chunk (caar a)))
          (car a)
          a))))


  (define-syntax define-registers
    (syntax-rules ()
      ((_ (pred code) r ...)
       (begin (define r 'r) ...
              (define set '(r ...))
              (define (pred x) (memq x set))
              (define (code x) (- (length set) (length (pred x))))))))

  (define-registers (register? register-code)
     r0  r1  r2  r3  r4  r5  r6  r7  r8  r9  rA  rB  rC  rD  rE  rF
    r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r1A r1B r1C r1D r1E r1F
    r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r2A r2B r2C r2D r2E r2F
    r30 r31 r32 r33 r34 r35 r36 r37 r38 r39 r3A r3B r3C r3D r3E r3F
    r40 r41 r42 r43 r44 r45 r46 r47 r48 r49 r4A r4B r4C r4D r4E r4F
    r50 r51 r52 r53 r54 r55 r56 r57 r58 r59 r5A r5B r5C r5D r5E r5F
    r60 r61 r62 r63 r64 r65 r66 r67 r68 r69 r6A r6B r6C r6D r6E r6F
    r70 r71 r72 r73 r74 r75 r76 r77 r78 r79 r7A r7B r7C r7D r7E r7F
    r80 r81 r82 r83 r84 r85 r86 r87 r88 r89 r8A r8B r8C r8D r8E r8F
    r90 r91 r92 r93 r94 r95 r96 r97 r98 r99 r9A r9B r9C r9D r9E r9F
    rA0 rA1 rA2 rA3 rA4 rA5 rA6 rA7 rA8 rA9 rAA rAB rAC rAD rAE rAF
    rB0 rB1 rB2 rB3 rB4 rB5 rB6 rB7 rB8 rB9 rBA rBB rBC rBD rBE rBF
    rC0 rC1 rC2 rC3 rC4 rC5 rC6 rC7 rC8 rC9 rCA rCB rCC rCD rCE rCF
    rD0 rD1 rD2 rD3 rD4 rD5 rD6 rD7 rD8 rD9 rDA rDB rDC rDD rDE rDF
    rE0 rE1 rE2 rE3 rE4 rE5 rE6 rE7 rE8 rE9 rEA rEB rEC rED rEE rEF
    rF0 rF1 rF2 rF3 rF4 rF5 rF6 rF7 rF8 rF9 rFA rFB rFC rFD rFE rFF)


  (define (group? x)
    (and (list? x)
         (for-all register? x)
         (or (< (length x) 2)
             (apply = (map (lambda (r) (div (register-code r) chunk-wsz)) x)))))

  (define group list)

  (define (encode-group/left g)
    (define (gr r) (* chunk-wsz (div (register-code r) chunk-wsz)))
    (define (gi r) (mod (register-code r) chunk-wsz))
    (list (if (null? g) 0 (gr (car g)))
          (fold-left (lambda (sel r) (bitwise-copy-bit sel (gi r) 1))
                     0 g)))

  (define (encode-group/right g)
    (let ((x (encode-group/left g)))
      (list (cadr x) (car x))))


  (define label-ref? symbol?)
  (define (maybe-label x) (if (label-ref? x) `(label ,x) x))


  (define-syntax define-constructors
    (syntax-rules ()
      ((_ (name ((operand pred) ...) . body) ...)
       (begin
         (define (name operand ...)
           (define who 'name)
           (check-arg operand pred who) ...
           (accum!/mark `(name . ,(let () . body))))
         ...))))

  (define (check-arg operand pred who)
    (unless (pred operand) (error who "invalid argument" operand)))

  (define (accum!/mark form)
    (accum! form)
    (make-marked form))

  ; This exists to support validity checking of chunk forms of the high-lang.
  ; Only datums that are valid low-lang <form>s may be marked.
  (define-record-type marked (fields value))


  (define-constructors

    (pointer ((val valid-id?)) (list val))

    ; This is only for defining labels at top-level.
    (label ((name symbol?)) (list name))

    ;---------------------------------------------------------------------------

    (ignore () '())

    (chunk-create ((dest register?) (src group?))
     (cons (register-code dest) (encode-group/right src)))

    (chunk-get ((dest group?) (src register?))
     (append (encode-group/left dest) (list (register-code src))))

    (set-multiple-immediates ((dest group?))
     (encode-group/left dest))

    (set-immediate ((dest register?) (val (or? signed-32bit? label-ref?)))
     ; Note that labels used with this operation aren't allowed to be greater
     ; than signed-32bit max.  (The low language will detect that.)
     (list (register-code dest) (maybe-label val)))

    (copy ((dest register?) (src register?))
     (map register-code (list dest src)))

    (ior ((dest register?) (src1 register?) (src2 register?))
     (map register-code (list dest src1 src2)))

    (sub ((dest register?) (src1 register?) (src2 register?))
     (map register-code (list dest src1 src2)))

    (jump ((index (or? unsigned-48bit? label-ref?)))
     (list (maybe-label index)))

    (jump-zero ((test register?) (index (or? unsigned-32bit? label-ref?)))
     (list (register-code test) (maybe-label index)))

    (jump-positive ((test register?) (index (or? unsigned-32bit? label-ref?)))
     (list (register-code test) (maybe-label index)))

    (jump-negative ((test register?) (index (or? unsigned-32bit? label-ref?)))
     (list (register-code test) (maybe-label index)))

    (goto ((segment register?))
     (list (register-code segment))))


  (define (data val)
    (check-arg val word-integer? 'data)
    (accum!/mark val))


  (define (chunk* . fields)
    `(chunk . ,(list-extend (map (lambda (x)
                                   (cond ((marked? x) (marked-value x))
                                         ((word-integer? x) x)
                                         ((label-ref? x) `(label ,x))
                                         (else (error 'chunk "invalid argument" x))))
                                 fields)
                            chunk-wsz 0)))

  (define-syntax chunk
    ; This syntax prevents any existing accumulations from being affected by
    ; evaluating the arguments to the chunk constructor.  This allows using the
    ; same accumulating constructors to make the arguments, which seems cleaner
    ; in terms of the high language syntax.
    (syntax-rules ()
      ((_ expr ...)
       (accum!/mark (apply chunk* (begin (start-accum!)
                                         (let ((l (list expr ...)))
                                           (end-accum!)
                                           l)))))))

  (define-syntax assemble
    ; This syntax prevents any existing accumulations from being affected by
    ; evaluating the arguments.
    (syntax-rules ()
      ((_ . body)
       (accum!/mark `(assemble . ,(eval/accum! . body))))))


  (define-syntax eval/accum!
    (syntax-rules ()
      ((_) '())
      ((_ . body)
       (begin (start-accum!)
              (let () . body)
              (end-accum!)))))

)
