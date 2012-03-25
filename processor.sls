#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library implements an emulated processor.  It should be used after
; forking a process for a processor, except special uses like the assembler.

; TODO?: Separate into libraries the stuff that's useful to other things like an
; assembler, e.g. (vifne processor registers), (vifne processor operations), and
; (vifne processor array). - Might be possible if send* and receive* can be
; passed to the sub-libraries via some initialization procedure (called in
; start-processor) each sub-library would provide.  Separating the libraries
; would also have the benefit of allowing the assembler to import what it needs
; without initializing the cache data structures that consume around 1.5 MB.

; TODO: How to handle user-caused processor exceptions?

(library (vifne processor)
  (export
    start-processor
    operation-code-bitsize
    get-operation-info
    operation-info-code
    operation-info-operand-bitsizes
    operation-info-operand-validators)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs lists)
    (rnrs exceptions)
    (rnrs conditions)
    (rnrs records syntactic)
    (rnrs arithmetic bitwise)
    (rnrs hashtables)
    (vifne config)
    (vifne posix)
    (vifne message-queue)
    (prefix (only (vifne storage) load-chunk store-chunk!) storage:)
    (only (vifne storage) valid-id?))

  (define sid)
  (define storage)
  (define replies)
  (define startup-tasks-head-id)
  (define startup-tasks-tail-id)

  (define (send* x) (send storage (cons* (car x) sid (cdr x))))
  (define (receive*) (receive replies))

  (define (start-processor n sth stt)
    ; Create this processor's message queue for replies from the storage
    ; controller.
    (define rmqn (string-append "processor" (number->string n)))
    (set! replies (create-message-queue rmqn))
    ; If the storage controller process has not created its message queue yet,
    ; wait a second and try again.
    (let retry ((max 5))
      (unless (positive? max)
        (error 'start-processor "storage-controller queue does not exist"))
      (guard (ex ((error? ex)
                  (sleep 1)
                  (retry (- max 1))))
        (set! storage (open-message-queue "storage-controller"))))
    ; Register with the storage controller.
    (send storage `(processor ,rmqn))
    ; Get the ID the storage controller assigned to this processor.
    (set! sid (cadr (receive*)))
    ; Save the chunk IDs of the startup-tasks stream, for when the processor
    ; stops.
    (set! startup-tasks-head-id sth)
    (set! startup-tasks-tail-id stt)
    ; Start executing instruction.
    (send* `(stream-get ,startup-tasks-head-id #F))
    (let ((x (receive*)))
      (if (list? x)
        (begin (assert (cadr x)) ; ptr? is true
               ; Set the Instruction Segment register to point to the
               ; instruction segment gotten from the stream, but don't increment
               ; the reference count because stream-get already incremented it.
               (set-register! (sr IS) (car x) #T #F)
               ;(register-value-set! (sr II) 0)  Already initialized.
               (instruction-interpreter))
        (begin (assert (eq? 'stream-empty x))
               ; TODO?: Wait for tasks to become available via stealing from
               ; another processor's PTQ or from the global DTQ.
               ))))

  ;-----------------------------------------------------------------------------

  (define-condition-type &processor-exception &serious
    make-processor-exception processor-exception?
    (type processor-exception-type))

  (define (processor-exception type) (raise (make-processor-exception type)))

  ; TODO?: Define and export bindings for the exception types?

  ;-----------------------------------------------------------------------------

  (define-record-type register (fields (mutable value) (mutable pointer?)))
  (define-record-type special-register (parent register) (fields validator))

  (define register-set
    (let ((v (make-vector register-set-size)))
      (do ((i 0 (+ 1 i)))
          ((= register-set-size i))
        (vector-set! v i (make-register 0 #F)))
      v))

  (define-syntax define-special-registers
    (syntax-rules ()
      ((_ set (special validator) ...)
       (begin (define set (vector (make-special-register 0 #F validator) ...))
              (define ordered '(special ...))
              (define special (- (length ordered) (length (memq 'special ordered))))
              ...))))

  (define pointer? register-pointer?)
  (define (non-pointer? r) (not (pointer? r)))

  (define-special-registers special-register-set
    (IS     pointer?)                 ; Instruction Segment
    (II     non-pointer?)             ; Instruction Index
    )

  (define (set-register! r v p? incr?)
    (assert ((if p? valid-id? non-negative-word-integer?) v))
    (let ((oldv (register-value r))
          (oldp? (register-pointer? r)))
      (register-value-set! r v)
      (register-pointer?-set! r p?)
      (when (and p? incr?) (send* `(increment ,v)))
      (when oldp? (send* `(decrement ,oldv)))))

  (define (r n) (vector-ref register-set n))
  (define (rv n) (register-value (r n)))
  (define (rp? n) (register-pointer? (r n)))
  (define (r-set! n v p? incr?) (set-register! (r n) v p? incr?))

  (define (sr n) (vector-ref special-register-set n))
  (define (srv n) (register-value (sr n)))
  (define (srp? n) (register-pointer? (sr n)))
  (define (sr-copy! s x)
    (let ((s (sr s)) (x (r x)))
      (unless ((special-register-validator s) x)
        (processor-exception 'invalid-special-register-value))
      (set-register! s (register-value x) (register-pointer? x) #T)))

  ;-----------------------------------------------------------------------------

  (define (array-ref handle-id index get-chunk)
    (define depth-field 0)
    (define tree-field  1)
    (let ((h (get-chunk handle-id)))
      (let-values (((depth dp?)   (chunk-ref h depth-field))
                   ((tree-id tp?) (chunk-ref h tree-field)))
        (when (or dp? (not tp?)) (processor-exception 'invalid-array))
        (let loop ((d depth) (i index) (t tree-id))
          (if (zero? d)
            (if (< i chunk-wsz)
              ; Note that the index is assumed to be valid if it's within a
              ; chunk at the specified depth.  It's the user's responsibility to
              ; know if an elements-chunk is not entirely used.
              (chunk-ref (get-chunk t) i)
              ; This bounds check is only done to prevent the processor from
              ; getting stuck in an unrecoverable state.  It does not guarantee
              ; the index is in-bounds (see above).
              (processor-exception 'out-of-bounds))
            (let*-values (((i r) (div-and-mod i (expt chunk-wsz d)))
                          ((t tp?) (chunk-ref (get-chunk t) i)))
              (unless tp? (processor-exception 'out-of-bounds))
              (loop (- d 1) r t)))))))

  (define (data-array-ref id i) (array-ref id i get-data-chunk))
  (define (inst-array-ref id i) (array-ref id i get-inst-chunk))

  (define (get-inst id i)
    (let-values (((inst-word ptr?) (inst-array-ref id i)))
      (when ptr? (processor-exception 'invalid-instruction))
      inst-word))

  (define (instruction-interpreter)
    (assert (srp? IS))
    (assert (not (srp? II)))
    (let ((id (srv IS)) (i (srv II)))
      (guard (ex #;((processor-exception? ex) TODO))
        (operation (get-inst id i))))
    (register-value-set! (sr II) (+ 1 (srv II)))
    (instruction-interpreter))

  ;-----------------------------------------------------------------------------

  (define operation-code-bitsize 16)

  (define (operation inst-word)
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

  ; Operand validators:
  (define (register-code? x)
    (and (exact-non-negative-integer? x) (< x register-set-size)))
  (define (special-register-code? x)
    (and (exact-non-negative-integer? x) (< x (vector-length special-register-set))))
  (define (group-mask? x)
    (and (exact-non-negative-integer? x) (<= x #xFFFF)))

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

  ; This record type represents a chunk in processor-local memory.
  (define-record-type chunk (fields id fields pointer-flags))

  (define (new-chunk id)
    (make-chunk id (make-vector chunk-wsz 0) (make-vector chunk-wsz #F)))

  (define (chunk-ref c i)
    (values (vector-ref (chunk-fields c) i)
            (vector-ref (chunk-pointer-flags c) i)))

  (define (chunk-set! c i v p?)
    (vector-set! (chunk-fields c) i v)
    (vector-set! (chunk-pointer-flags c) i p?))

  (define (load-chunk id)
    ; Copy a chunk from shared storage.
    (let ((f&p (storage:load-chunk id)))
      (if f&p (apply make-chunk id f&p) (processor-exception 'guarded))))

  (define (store-chunk! c)
    ; Copy a chunk to shared storage.
    (storage:store-chunk! (chunk-id c) (chunk-fields c) (chunk-pointer-flags c)))

  (define data-access-unit (make-vector cache-size #F))
  (define inst-access-unit (make-vector cache-size #F))
  (define data-assoc-unit (make-eqv-hashtable cache-size))
  (define inst-assoc-unit (make-eqv-hashtable cache-size))

  ; The Least-Recently-Used data structure is a mutable double-linked list of
  ; nodes that represent cache locations, and a node lookup table indexed by
  ; cache locations, and references to the current least- and most-recently-used
  ; nodes.  The list is ordered from least- to most-recently-used.  An LRU is
  ; updated when a cache location is accessed, such that the corresponding node
  ; becomes the most-recently-used, and a new node may become the
  ; least-recently-used.

  (define-record-type LRU (fields table (mutable least) (mutable most)))
  (define-record-type LRU-node (fields index (mutable prev) (mutable next)))

  (define (new-LRU)
    (let ((t (make-vector cache-size)))
      (do ((i 0 (+ 1 i)))
          ((<= cache-size i))
        (let* ((prev (and (positive? i) (vector-ref t (- i 1))))
               (n (make-LRU-node i prev #F)))
          (vector-set! t i n)
          (when prev (LRU-node-next-set! prev n))))
      (make-LRU t 0 (vector-ref t (- (vector-length t) 1)))))

  (define data-lru (new-LRU))
  (define inst-lru (new-LRU))

  (define (update-LRU! lru i)
    ; Make a cache location's node be the most-recently-used in the LRU list.
    ; Also possibly set a new least-recently-used location.
    (let* ((i (vector-ref (LRU-table lru) i))
           (n (LRU-node-next i)))
      (when n
        (let ((p (LRU-node-prev i)))
          (if p
            (LRU-node-next-set! p n)
            (LRU-least-set! lru (LRU-node-index n)))
          (LRU-node-prev-set! n p)
          (let ((m (LRU-most lru)))
            (LRU-node-prev-set! i m)
            (LRU-node-next-set! i #F)
            (LRU-node-next-set! m i)
            (LRU-most-set! lru i))))))

  (define (cache-chunk c cache table lru)
    (let* ((i (LRU-least lru))
           (old (vector-ref cache i)))
      ; Reference counts must be incremented when chunks are cached, to prevent
      ; IDs from being allocated if they're still in any processor's cache.
      (send* `(increment ,(chunk-id c)))
      (when old
        (hashtable-delete! table (chunk-id old))
        (send* `(decrement ,(chunk-id old))))
      (vector-set! cache i c)
      (hashtable-set! table (chunk-id c) i)
      (update-LRU! lru i)))

  (define (get-chunk id cache table lru)
    (let ((i (hashtable-ref table id #F)))
      (if i
        (begin (update-LRU! lru i)
               (vector-ref cache i))
        (let ((c (load-chunk id)))
          (cache-chunk c cache table lru)
          c))))

  (define (get-data-chunk id) (get-chunk id data-access-unit data-assoc-unit data-lru))
  (define (get-inst-chunk id) (get-chunk id inst-access-unit inst-assoc-unit inst-lru))

  ; TODO?: Cache of new chunks ready to be allocated?  Will have to free them
  ; when processor stops.

  ;-----------------------------------------------------------------------------

  ; This library assumes a word size of 64 bits and 16 words per chunk.

  (define max-16bit (expt 2 16))

  (assert (= 8 word-size))
  (assert (= 16 chunk-wsz))

  (assert (exact-positive-integer? register-set-size))
  (assert (<= register-set-size max-16bit))
  (assert (zero? (mod register-set-size chunk-wsz)))

  (assert (<= (vector-length special-register-set) max-16bit))
  (assert (<= (vector-length operations-table) max-16bit))

)
