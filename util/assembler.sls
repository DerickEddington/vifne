#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library provides an assembler, that operates outside the emulator, that
; enables constructing data structures, especially instruction segments, in an
; emulator storage file, including the ability to bypass capability-security
; with magic pointers.  Its assembly language is syntactically made of Scheme
; datums ... TODO more.

(library (vifne util assembler)
  (export
    assemble-high
    assemble-low
    list->array-tree
    #;array-tree->list)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs lists)
    (rnrs hashtables)
    (rnrs arithmetic bitwise)
    (rnrs eval)
    (rnrs exceptions)
    (vifne config)
    (only (vifne storage) valid-id?)
    (vifne processor operations)
    (except (vifne util assembler storage) chunk?))

  (define (named-list? name size x)
    (and (list? x) (pair? x)
         (eq? name (car x))
         (= size (length x))))


  ; The low-level assembly language consists of:
  ;
  ;   <source> : <segment> | <chunk>
  ;   <segment> : (<form> ...)
  ;   <form> : <instruction> | <data> | <label>
  ;   <instruction> : (<mnemonic-symbol> <operand> ...)
  ;   <operand> : <exact-integer> | <label>
  ;   <data> : <word-integer> | (pointer <id-integer>) | <chunk> | <recur>
  ;   <chunk> : (chunk <form> ...)
  ;   <recur> : (assemble . <source>)
  ;   <label> : (label <symbol>)
  ;
  ; It is designed to be representable as standard textual datums (e.g. in
  ; files), and to support both instruction and data definition.
  ;
  ; A <source> that is a <segment> means to assemble an instruction-segment
  ; array-tree.
  ;
  ; A <source> that is a <chunk> means to assemble an arbitrary graph of chunks.
  ;
  ; An <instruction> means to encode the instruction as a non-pointer integer
  ; value for the corresponding chunk field.
  ;
  ; A <data> that is a <word-integer> means a non-pointer literal integer value
  ; for the corresponding chunk field.  A <data> that is a (pointer <id-integer>)
  ; means a "magic" synthesized pointer value for the corresponding chunk field,
  ; used to bypass capability-security (this must be explicitly enabled
  ; elsewhere, to store it in a storage file).  Allowing <data>s in instruction
  ; segments is intended for supporting immediate values (it could also be used
  ; to specify literal encoded instructions).
  ;
  ; A <chunk> means to construct a new graph of new chunks, and to make the
  ; value for the corresponding chunk field be a pointer to it.  <chunk> graphs
  ; may occur in instruction segments.  And, any form, including instructions,
  ; may occur in <chunk> graphs.
  ;
  ; A <recur> means to recursively assemble a new chunk graph from an embedded
  ; assembly, and to make the value for the corresponding chunk field be a
  ; pointer to it.  This is especially useful for defining multiple
  ; instruction-segments (e.g. internal procedures) from one assembly.
  ;
  ; An assembly may contain multiple identical <chunk>s or <recur>s, to
  ; represent multiple references to the same chunk (but cycles aren't allowed),
  ; and this is denoted with <chunk> or <recur> datums that are eq?, and this
  ; can be achieved in textual datums via (semi-standard) reader graph syntax,
  ; e.g. (chunk #0=(chunk ---) #0# ---).  Be careful to not cause cycles in the
  ; datums, because that will cause algorithms to recur infinitely.
  ;
  ; A <label> names an index (offset from beginning) in an instruction segment.
  ; A <label> at the top level of a <segment> means to define a new label bound
  ; to the following index.  Such labels do not occur in the assembled output,
  ; and so do not affect the size nor indexes of a segment.  A <label> in an
  ; <instruction> or <chunk> means a reference to a label, and is replaced with
  ; the index of the label.  Labels in a <recur> are relative to the recursive
  ; assembly.


  (define (assemble-low source)
    ; This is the low-level assembler.  It translates low-level assembly
    ; language forms into an encoded instruction-segment array-tree, or into an
    ; arbitrary graph, represented as a graph of chunk records.
    (define (die msg . a) (apply error 'assemble-low msg a))

    (define (form? x) (or (instruction? x) (data? x) (label? x)))

    (define (instruction? x)
      (and (list? x) (pair? x)
           (symbol? (car x))
           (not (memq (car x) '(pointer chunk assemble label)))
           (for-all (lambda (o)
                      (or (exact-integer? o)
                          (label? o)
                          (die "invalid operand" o x)))
                    (cdr x))))

    (define (data? x)
      (or (and (exact-integer? x)
               (or (word-integer? x) (die "not in word range" x)))
          (pointer? x)
          (chunk? x)
          (recur? x)))

    (define (pointer? x)
      (and (named-list? 'pointer 2 x)
           (or (valid-id? (cadr x))
               (die "invalid pointer number" (cadr x) x))))

    (define (chunk? x)
      (and (named-list? 'chunk (+ 1 chunk-wsz) x)
           (for-all (lambda (f) (or (form? f) (die "invalid form" f)))
                    (cdr x))))

    (define (recur? x)
      ; Note: validation of the rest is done in the recursive call to assemble.
      (and (pair? x) (eq? 'assemble (car x))))

    (define (label? x)
      (and (named-list? 'label 2 x)
           (symbol? (cadr x))))

    (define (labels->indexes segment)
      (define (defs)
        (cdr (fold-left (lambda (a form)
                          (let ((i (car a)) (l (cdr a)))
                            (if (label? form)
                              (if (assq (cadr form) l)
                                (die "label already defined" form)
                                (cons* i (cons (cadr form) i) l))
                              (cons (+ 1 i) l))))
                        '(0) segment)))
      (define (replace defs)
        (define (L x)
          (if (label? x)
            (cond ((assq (cadr x) defs) => cdr)
                  (else (die "undefined label" x)))
            x))
        (define (R form)
          (cond ((instruction? form)
                 `(,(car form) . ,(map L (cdr form))))
                ((chunk? form)
                 ; TODO: Doesn't this break supporting identical references,
                 ; because the eq? identity of chunk forms is lost.
                 `(chunk . ,(map (lambda (f) (if (or (chunk? f) (instruction? f))
                                               (R f)
                                               (L f)))
                                 (cdr form))))
                (else form)))
        (map R (remp label? segment)))
      (replace (defs)))

    ; This table maps <chunk> and <recur> datums to their corresponding chunk
    ; records, to support chunks referred to more than once.
    (define CT (make-eq-hashtable))

    (define (transform form)
      (define (err msg . a) (apply die msg (append a (list form))))

      (define (encode)
        (let ((info (get-operation-info (car form)))
              (operands (cdr form)))
          (unless info (err "invalid mnemonic" (car form)))
          ; Validate the operands.
          (let ((v (operation-info-operand-validators info)))
            (unless (= (length v) (length operands))
              (err "invalid amount of operands"))
            (for-each (lambda (v o) (unless (v o) (err "invalid operand" o)))
                      v operands))
          ; Encode the operation into a number.
          (let loop ((code (operation-info-code info))
                     (shift operation-code-bitsize)
                     (operands operands)
                     (sizes (operation-info-operand-bitsizes info)))
            (if (null? operands)
              code
              (loop (bitwise-ior code
                                 (bitwise-arithmetic-shift-left
                                  ; In case the operand is too big or negative,
                                  ; truncate it and/or make it non-negative.
                                  (bitwise-bit-field (car operands) 0 (car sizes))
                                  shift))
                    (+ (car sizes) shift)
                    (cdr operands)
                    (cdr sizes))))))

      (define (data-trans)
        (define (do-chunk proc)
          (or (hashtable-ref CT form #F)
              (let ((c (proc)))
                (hashtable-set! CT form c)
                c)))
        (if (integer? form) form
            (case (car form)
              ((pointer) (new-magic-pointer (cadr form)))
              ((chunk) (do-chunk (lambda () (new-chunk (map transform (cdr form))))))
              ((assemble)
               ; Recurring with assemble (not assemble-low) is done to use the
               ; same CT hashtable across recursive assemblies, to support
               ; identical chunk references across recursive assemblies.
               (do-chunk (lambda () (assemble (cdr form))))))))

      (cond
        ((data? form) (data-trans))
        ((instruction? form) (encode))
        (else (err "invalid form"))))

    (define (assemble source)
      (cond
        ((and (pair? source) (eq? 'chunk (car source)))
         (transform source))
        ((list? source)
         (list->array-tree (map transform (labels->indexes source))))
        (else (die "invalid source" source))))

    (assemble source))


  (define (list->array-tree fields)
    ; Construct an array-tree, represented as chunk records, of the type used by
    ; a processor for instruction-segments, from a list.  The elements must be
    ; types that new-chunk accepts.

    (define (depth x)
      (let-values (((x r) (div-and-mod x chunk-wsz)))
        (let ((x (if (zero? r) x (+ 1 x))))
          (if (< 1 x)
            (+ 1 (depth x))
            0))))

    (define (make-tree d l)
      (define (take-fields)
        (do ((i chunk-wsz (- i 1))
             (a '() (cons (car l) a))
             (l l (cdr l)))
            ((or (zero? i) (null? l)) (values (reverse a) l))))
      (if (zero? d)
        (let-values (((f l) (take-fields)))
          (assert (not (null? f)))
          (values (new-chunk f) l))
        (let loop ((f '()) (l l) (i chunk-wsz))
          (if (or (zero? i) (null? l))
            (begin (assert (not (null? f)))
                   (values (new-chunk (reverse f)) l))
            (let-values (((t l) (make-tree (- d 1) l)))
              (loop (cons t f) l (- i 1)))))))

    (define (make-handle . a) (new-chunk a))

    ; Compute the size and depth, and make the tree and the handle.
    (let ((size (length fields)))
      (assert (positive? size))
      (let ((d (depth size)))
        (let-values (((t l) (make-tree d fields)))
          (assert (null? l))
          (make-handle d t)))))


  ; Will be used by disassembler:
#;(define (array-tree->list )
    )


  (define (assemble-high forms)
    ; This is the high-level assembler.  It enables the high-level assembly
    ; language that supports full use of Scheme.  The forms are expected to use
    ; the (vifne util assembler high-lang) library, directly or indirectly, to
    ; accumulate low-level forms.
    (define (die msg . a) (apply error 'assemble-high msg a))
    (define-syntax try
      (syntax-rules ()
        ((_ expr msg)
         (with-exception-handler
           (lambda (e) (die msg e forms))
           (lambda () expr)))))
    (assert (list? forms))
    (let* ((imports (and (pair? forms)
                         (pair? (car forms))
                         (eq? 'import (caar forms))
                         (cdar forms)))
           (forms (if imports (cdr forms) forms))
           (env (apply environment
                       '(only (vifne util assembler high-lang) eval/accum!)
                       (or imports '()))))
      (try (eval `(eval/accum! . ,forms)
                 env)
           "evaluation exception")))

  ;-----------------------------------------------------------------------------

  ; The special forms of the low-level language must not conflict with defined
  ; instruction operation mnemonics.
  (assert (not (exists get-operation-info '(pointer chunk assemble label))))

)
