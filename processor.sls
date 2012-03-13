#!r6rs
; Copyright 2012 Derick Eddington.  My MIT-style license is in the file named
; LICENSE from the original collection this file is distributed with.

; This library is the emulated processor.  This library must be used only after
; forking a process for a processor.

(library (vifne processor)
  (export
    start-processor)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs lists)
    (rnrs exceptions)
    (rnrs conditions)
    (rnrs records syntactic)
    (rnrs arithmetic bitwise)
    (vifne config)
    (vifne posix)
    (vifne message-queue)
    (vifne processor cache)
    (vifne processor exception))

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
               (register-value-set! (sr IS) (car x))
               (register-pointer?-set! (sr IS) #T)
               ;(register-value-set! (sr II) 0)  Already initialized.
               (instruction-interpreter))
        (begin (assert (eq? 'stream-empty x))
               ; TODO?: Wait for tasks to become available via stealing from
               ; another processor's PTQ or from the global DTQ.
               ))))


  ; TODO?: Seperate into libraries the stuff that's useful to other things like
  ; an assembler, e.g. (vifne processor registers), (vifne processor
  ; operations), and (vifne processor array).


  (define-record-type register (fields (mutable value) (mutable pointer?) (mutable mutable?)))
  (define-record-type special-register (parent register) (fields validator))

  (define register-set
    (do ((i register-set-size (- i 1))
         (l '() (cons (make-register 0 #F #F) l)))
        ((zero? i) (list->vector l))))

  #;(define (always-true _) #T)

  (define-syntax define-special-registers
    (syntax-rules (+ vector)
      ((_ (+ . n) (vector regs ...) set (special validator) . r)
       (begin (define special (+ . n))
              (define-special-registers (+ 1 (+ . n))
                (vector regs ... (make-special-register 0 #F #F validator))
                set . r)))
      #;((_ (+ . n) (vector . v) s special . r)
         (define-special-registers (+ . n) (vector . v) s (special always-true) . r))
      ((_ _ (vector . regs) set)
       (define set (vector . regs)))
      ((_ . r)
       (define-special-registers (+ 0) (vector) . r))))

  (define (pointer?-immutable? r) (and (register-pointer? r) (not (register-mutable? r))))
  (define (non-pointer? r) (not (register-pointer? r)))

  ; TODO: The Trip Handler Table doesn't seem good.  Maybe trips aren't good.
  ; How to handle user-caused processor exceptions?

  (define-special-registers special-register-set
    (IS     pointer?-immutable?)      ; Instruction Segment
    (II     non-pointer?)             ; Instruction Index
    #|(TH     pointer?-immutable?)      ; Trip Handler Table
    (TS     pointer?-immutable?)      ; Trip Instruction Segment
    (TI     non-pointer?)             ; Trip Instruction Index |#
    )

  (define (r n) (vector-ref register-set n))
  (define (rv n) (register-value (r n)))
  (define (rv-set! n v) (register-value-set! (r n) v))
  (define (rp? n) (register-pointer? (r n)))
  (define (rp?-set! n v) (register-pointer?-set! (r n) v))
  (define (rm? n) (register-mutable? (r n)))
  (define (rm?-set! n v) (register-mutable?-set! (r n) v))

  (define (sr n) (vector-ref special-register-set n))
  (define (srv n) (register-value (sr n)))
  (define (srp? n) (register-pointer? (sr n)))
  (define (srm? n) (register-mutable? (sr n)))
  (define (sr-copy! s x)
    (let ((s (sr s)) (x (r x)))
      (unless ((special-register-validator s) x)
        (processor-exception 'invalid-special-register-value))
      (register-value-set! s (register-value x))
      (register-pointer?-set! s (register-pointer? x))
      (register-mutable?-set! s (register-mutable? x))))

  (define (group-select reg sel)
    (let ((group (bitwise-and #xFFF0 reg)))
      (let loop ((i (- chunk-wsz 1)) (r '()) (n '()))
        (if (negative? i)
          (values r n)
          (if (bitwise-bit-set? sel i)
            (loop (- i 1) (cons (+ group i) r) (cons i n))
            (loop (- i 1) r n))))))


  (define (array-ref head-id index get-chunk)
    'TODO
    #;(let ((h (get-chunk head-id)))
      (let-values (((size p?) (chunk-ref h ?)))
        (when p? (processor-exception 'invalid-array))
        (let ((depth ?))
          (find the right place or error)
          (chunk-ref c i)))))

  (define (data-array-ref id i) (array-ref id i get-data-chunk))
  (define (inst-array-ref id i) (array-ref id i get-inst-chunk))


  (define (get-inst id i)
    (let-values (((inst-word ptr?) (inst-array-ref id i)))
      (when ptr? (processor-exception 'invalid-instruction))
      inst-word))

  (define (operation inst-word)
    (let ((opcode (bitwise-and #xFFFF inst-word)))
      (unless (< opcode (vector-length operations-table))
        (processor-exception 'invalid-instruction))
      ((vector-ref operations-table opcode) inst-word)))

  #;(define (trip-handler ex-num)
    (assert (srp? TH))
    (let-values (((handler-id ptr?) (data-array-ref (srv TH) ex-num)))
      ; If trying to use the trip-handler-table array
      ; causes a processor exception during trip-handler?  Shouldn't
      ; just do a processor-exception for that, because the
      ; trip-handler-table array shouldn't be used again (because
      ; it's invalid).
      (assert ptr?)
      handler-id))

  (define (instruction-interpreter)
    (assert (srp? IS))
    (assert (not (srp? II)))
    (let ((id (srv IS)) (i (srv II)))
      (register-value-set! (sr II) (+ 1 i))
      (guard (ex #;((processor-exception? ex)
                    (srv-set! TS (srv IS))
                    (srv-set! TI (srv II))
                    (srv-set! IS (trip-handler (processor-exception-num ex)))
                    (srv-set! II 0)))
        (operation (get-inst id i))))
    (instruction-interpreter))


  (define-syntax define-operations
    (syntax-rules (+ vector)
      ((_ (+ . n) (vector p ...) t v (op . body) . r)
       (begin (define op (+ . n))
              (define-operations (+ 1 (+ . n))
                (vector p ... (lambda (v) . body))
                t v . r)))
      ((_ _ (vector . procs) table _)
       (define table (vector . procs)))
      ((_ . r)
       (define-operations (+ 0) (vector) . r))))

  (define-operations operations-table inst-word

    (new-chunks
     (let-values (((g _) (group-select (operand inst-word 0) (operand inst-word 1))))
       ; TODO?: If in the future there's a cache of chunks for allocating, this
       ; will change to use it, instead of communicating with the storage
       ; controller here.
       (send* `(allocate ,(length g)))
       (let ((ids (receive*)))
         (when (symbol? ids) (processor-exception ids))
         (when (< (length ids) (length g))
           (send* `(decrement . ,ids)) ; Free any that were allocated.
           (processor-exception 'storage-full))
         (for-each (lambda (r id) (rv-set! r id) (rp?-set! r #T) (rm?-set! r #T))
                   g ids))))

    (seal
     (let-values (((g _) (group-select (operand inst-word 0) (operand inst-word 1))))
       (for-each (lambda (r)
                   (unless (rp? r) (processor-exception 'not-pointer))
                   (seal-chunk! (get-data-chunk (rv r))))
                 g)))

    (chunk-get
     (let ((r (operand inst-word 2)))
       (unless (rp? r) (processor-exception 'not-pointer))
       (let ((c (get-data-chunk (rv r))))
         (let-values (((r i) (group-select (operand inst-word 0) (operand inst-word 1))))
           (for-each (lambda (r i)
                       (let-values (((v p?) (chunk-ref c i)))
                         (rv-set! r v)
                         (rp?-set! r p?)
                         (rm?-set! r #F)))
                     r i)))))

    (chunk-set!
     (let ((r (operand inst-word 0)))
       (unless (rp? r) (processor-exception 'not-pointer))
       (unless (rm? r) (processor-exception 'immutable))
       (let ((c (get-data-chunk (rv r))))
         (let-values (((r i) (group-select (operand inst-word 2) (operand inst-word 1))))
           (for-each (lambda (r i)
                       (when (and (rp? r) (rm? r)) (processor-exception 'mutable))
                       (chunk-set! c i (rv r) (rp? r)))
                     r i)))))

    (set-multiple-immediates
     TODO)

    (set-immediate
     (let ((r (operand inst-word 0)))
       (rv-set! r (bitwise-bit-field inst-word 32 64))
       (rp?-set! r #F)))

    (sign-extend
     TODO)

    (set-special
     (sr-copy! (operand inst-word 0) (operand inst-word 1)))

    #;(resume
     (let ((retry (operand inst-word 0)))
       (assert (srp? TS))
       (assert (not (srp? TI)))
       (srv-set! IS (srv TS))
       (if (positive? retry)
         (if (= 1 retry)
           ; Retry the tripped instruction.
           (srv-set! II (- (srv TI) 1))
           ; Pretend operand-1 was the tripped instruction and "retry" that.
           (operation (rv (operand inst-word 1))))
         ; Execute the instruction following the tripped one.
         (srv-set! II (srv TI))))))

  (define (operand inst-word i)
    (assert (<= 0 i 2))
    (let ((i (* 16 (+ 1 i))))
      (bitwise-bit-field inst-word i (+ 16 i))))

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
