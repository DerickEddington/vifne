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
    operations:initialize!
    activate
    activate-ready-task)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs lists)
    (rnrs records syntactic)
    (rnrs arithmetic bitwise)
    (rnrs hashtables)
    (rnrs programs)
    (vifne config)
    (only (vifne storage) store-chunk! f fv fp?)
    (only (vifne message-queue) send receive)
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
     ; Allocate a chunk and set the fields according to the register group
     ; selection.
     (let ((id (chunk-create (map (lambda (r) (if r (rf r) (f 0 #F)))
                                  (group-select src-grp grp-sel)))))
       ; Set the specified register to point to the chunk, but don't increment
       ; the reference count because the allocation already set it to 1.
       (r-set! dest (f id #T))))


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


  #;((equal? )
     #|TODO compare two registers, both value and pointerness,
     and store result in third register|#)


    ((ior (dest 16 register-code?)
          (src1 16 register-code?)
          (src2 16 register-code?))
     (arith bitwise-ior dest src1 src2))


    ((sub (dest 16 register-code?)
          (src1 16 register-code?)
          (src2 16 register-code?))
     (arith (lambda (a b) (let ((x (- a b))) (if (negative? x) (+ (expt 2 64) x) x)))
            dest src1 src2))


    ((jump (index 48 unsigned-48bit?))
     (sr-set! II (f index #F)))


  #;((jump-pointer? )
     #|TODO If the register value is a pointer, jump|#)


    ; TODO: These conditions are probably inadequate - they don't support
    ; overflow/underflow etc.

    ((jump-zero (test  16 register-code?)
                (index 32 unsigned-32bit?))
     (jump/? zero? test index))


    ((jump-positive (test  16 register-code?)
                    (index 32 unsigned-32bit?))
     (jump/? (lambda (v) (and (not (bitwise-bit-set? v 63)) (positive? v))) test index))


    ((jump-negative (test  16 register-code?)
                    (index 32 unsigned-32bit?))
     (jump/? (lambda (v) (bitwise-bit-set? v 63)) test index))


    ((goto (seg 16 register-code?))
     (unless (rp? seg) (processor-exception 'not-pointer))
     ; Enter only at index 0, to enforce capability security.
     (goto (rv seg) 0))


    ((call (seg   16 register-code?)
           (save0 16 register-code?)
           (save1 16 register-code?))
     (unless (rp? seg) (processor-exception 'not-pointer))
     ; Set the Return special register to point to the callee's Return Chunk,
     ; but don't increment the reference count because the allocation already
     ; set it to 1.
     (sr-set! R (f (create-return-chunk save0 save1) #T))
     (goto (rv seg) 0))


    ((return)
     (unless (srp? R) (processor-exception 'not-pointer))
     ; Restore the saved datas into the registers the caller had them in.
     ; Prepare the control transfer to the return point.  Restore the caller's
     ; Return special register.
     (activate (srv R)))


    ((spawn (seg  16 register-code?)
            (arg0 16 register-code?)
            (arg1 16 register-code?))
     (unless (rp? seg) (processor-exception 'not-pointer))
     (spawn (create-task-chunk
             (rv seg) 0 ; Beginning of given instruction segment.
             (f 0 #F)   ; Return special register is null.
             (rf arg0)  ; Save the argument.
             arg0       ; Code of register to put argument into.
             (rf arg1)  ; Save the argument.
             arg1)))    ; Code of register to put argument into.


    ((done)
     (clear-registers!) ; To ensure chunks can be freed ASAP.
     (activate-ready-task))


    ((yield (save0 16 register-code?)
            (save1 16 register-code?))
     (spawn (create-return-chunk save0 save1))
     (activate-ready-task))


    ((stream-create (head 16 register-code?)
                    (tail 16 register-code?))
     (send* '(allocate-stream))
     (let ((x (receive*)))
       (when (symbol? x) (processor-exception x))
       ; Set the specified registers to point to the head and tail handles
       ; chunks, but don't increment the reference counts because the
       ; allocations already set them to 1.
       (r-set! head (f (cadr x) #T))
       (r-set! tail (f (caddr x) #T))))


    ((stream-put (tail 16 register-code?)
                 (val  16 register-code?))
     (unless (rp? tail) (processor-exception 'not-pointer))
     (send* `(stream-put ,(rv tail) ,(rf val)
                         ; Increment reference count of chunk pointed to by
                         ; value, if value is a pointer, and if value is put.
                         #T
                         ; Do not put the value, if there is a waiter.  Safe
                         ; even if emulator is in the process of stopping,
                         ; because processor operations cannot be interrupted.
                         #F))
     (let ((x (receive*)))
       (when (symbol? x) (processor-exception x))
       (let ((waiter (cadr x)))
         (when waiter
           (assert (fp? waiter))
           (let ((task-id (fv waiter)))
             ; There is a task waiting to be resumed with the put-value.  The
             ; value was not actually put in the stream, in this case.  Spawn
             ; the waiting task with the value.  This ensures that waiting tasks
             ; get put-values in the same order the tasks were queued.
             (load-fields task-id (is ii r _ a-reg saved s-reg)
               ; Mutate the task chunk to set the put-value as arg0.  Functional
               ; update, i.e. creating a new chunk derived from the task chunk
               ; but with arg0 set, is not done, to avoid the problem that
               ; allocating a new chunk might fail, after the waiting task has
               ; already been removed from the waiters queue.  No other
               ; processors can possibly have access to the task chunk (because
               ; only stream-get could have created it), so it's safe to
               ; directly write the mmap'ed memory here.  This is a convenience;
               ; otherwise the S.C. would have to do the mutation.
               (when (rp? val) (send* `(increment ,(rv val))))
               ; TODO?: Don't need to do store-chunk!, could use something like
               ; set-word! for the one mutated field, and not waste execution
               ; setting the other fields to their same values.  Can't use
               ; set-field! because it directly mutates the refcount in the
               ; mmap'ed storage file, which would be a concurrency bug.  Don't
               ; want to export set-word! (nor id->ptr).  Maybe change
               ; set-field! to take extra optional arg to tell it to not adjust
               ; the refcounts.
               (store-chunk! task-id (list is ii r (rf val) a-reg saved s-reg)))
             (spawn task-id))))))


    ((stream-get (dest 16 register-code?)
                 (head 16 register-code?)
                 (save 16 register-code?))
     (unless (rp? head) (processor-exception 'not-pointer))
     ; Create a task that will resume the current task at the next instruction.
     ; Maybe unneeded, but must create here in case.
     (let ((resume (create-return-chunk* (f 0 #F) dest (rf save) save)))
       ; Ask for the next element.  Enqueue the resume task if there isn't an
       ; available element, and tell stream-get to not increment the reference
       ; count of the resume task, if it does enqueue it, because the allocation
       ; already set it to 1.
       (send* `(stream-get ,(rv head) ,(f resume #T) #F))
       (let ((x (receive*)))
         (cond ((list? x)
                ; There is an available element, so set it in the dest register
                ; and let the next instruction execute.  Don't increment the
                ; reference count, if it's a pointer, because stream-get already
                ; did.
                (r-set! dest (cadr x) #F)
                ; Free the unneeded resume task chunk.
                (send* `(decrement ,resume)))
               ((eq? 'stream-empty x)
                ; Nothing more for the current task to do, so execute some other
                ; ready task.  The resume task will be spawned on some
                ; processor when an element becomes available.
                (activate-ready-task))
               (else (processor-exception x))))))

    ; TODO?: Keep this?  Created only to help with early test programs.
    ((exception (code 32 unsigned-32bit?))
     (processor-exception code))

    )


  (define-syntax load-fields
    (syntax-rules ()
      ((_ ("recur" f) (id . rest) . body)
       (let ((id (car f)) (r (cdr f)))
         (load-fields ("recur" r) rest . body)))
      ((_ ("recur" _) () . body)
       (let () . body))
      ((_ chunk (id ...) . body)
       (let ((f (load-chunk* chunk)))
         (load-fields ("recur" f) (id ...) . body)))))


  (define (chunk-create fields)
    ; Allocating a chunk must be done by messaging the storage controller, to
    ; serialize concurrent-processes' access to the storage's metadata.  The new
    ; chunk is returned with a reference count of 1.
    (send* '(allocate))
    (let ((x (receive*)))
      (when (symbol? x) (processor-exception x))
      ; Increment the reference counts of chunks referenced by the new chunk.
      ; This must be done by messaging the storage controller, to serialize
      ; concurrent access to those chunks by other processors.
      (for-each (lambda (f) (when (fp? f) (send* `(increment ,(fv f)))))
                fields)
      (let ((id (cadr x)))
        ; No other processors can possibly have access to the chunk yet (because
        ; it's new and hasn't been given to anything else), so it's safe to
        ; directly write the mmap'ed memory here.  This is an easy optimization,
        ; instead of sending the fields values to the S.C. and having it do the
        ; setting.
        (store-chunk! id fields)
        id)))


  (define (group-select reg sel)
    (let ((group (bitwise-and #xFFF0 reg)))
      (do ((i (- chunk-wsz 1) (- i 1))
           (r '() (cons (and (bitwise-bit-set? sel i) (+ group i)) r)))
          ((negative? i) r))))


  (define (arith proc dest src1 src2)
    (when (or (rp? src1) (rp? src2)) (processor-exception 'pointer))
    (r-set! dest (f (proc (rv src1) (rv src2)) #F)))


  (define (jump/? pred test index)
    (when (rp? test) (processor-exception 'pointer))
    (when (pred (rv test)) (sr-set! II (f index #F))))


  (define (goto seg idx)
    (sr-set! IS (f seg #T) #T)
    (sr-set! II (f idx #F)))


  (define (create-task-chunk is ii r arg0 a0-reg arg1 a1-reg)
    ; The chunk type that represents a task that can be activated.  The new
    ; chunk is returned with a reference count of 1.
    (chunk-create (list (f is #T)        ; Entry point instruction segment.
                        (f ii #F)        ; Entry point index.
                        r                ; Value for Return special register.
                        arg0             ; Argument.
                        (f a0-reg #F)    ; Code of register to put argument into.
                        arg1             ; Argument.
                        (f a1-reg #F)))) ; Code of register to put argument into.

  (define (create-return-chunk save0 save1)
    (create-return-chunk* (rf save0) save0 (rf save1) save1))

  (define (create-return-chunk* save0-f save0-r save1-f save1-r)
    (create-task-chunk
     (srv IS) (srv II) ; Next instruction is return point.
     (srf R)           ; Caller's Return special register to save and restore.
     save0-f           ; Caller's data to save and restore.
     save0-r           ; Code of register to restore saved data into.
     save1-f           ; Caller's data to save and restore.
     save1-r))         ; Code of register to restore saved data into.


  (define (activate task-id)
    ; Prepare control transfer specified by a task chunk.
    (load-fields task-id (is ii r arg0 a0-reg arg1 a1-reg)
    #;(assert (and (fp? is) (not (fp? ii))
                   (not (fp? a0-reg)) (register-code? (fv a0-reg))
                   (not (fp? a1-reg)) (register-code? (fv a1-reg))))
      ; Prepare the control transfer.
      (goto (fv is) (fv ii))
      ; Set the arguments.  (Having two is necessary, for resuming tasks waiting
      ; on stream-get, to both save data to restore and pass the gotten value.
      ; For other tasks, two is just sometimes more convenient.)
      (r-set! (fv a0-reg) arg0 #T)
      (r-set! (fv a1-reg) arg1 #T)
      ; Set the Return special register.  This might lose the only reference to
      ; the previous R. chunk, which will decrement the reference counts of the
      ; chunks it referenced.  Setting the registers above (including the goto)
      ; must be done first, in case the previous R. chunk held the only
      ; reference to a chunk that needs to be referenced by one of those
      ; registers.
      (sr-set! R r #T)))


  (define (spawn task-id)
    ; Put it in the Ready-Tasks stream, so any processor can take it.  The task
    ; chunk is assumed to already have reference count = 1, and assumed to not
    ; be referenced by anything else, so tell stream-put to not increment the
    ; reference count.
    (send* `(stream-put ,ready-tasks-tail ,(f task-id #T)
                        ; Do not increment reference count of chunk pointed to
                        ; by value.
                        #F
                        ; Put the value, even if there is a waiter.  Ensures the
                        ; task cannot be lost if the emulator is in the process
                        ; of stopping.
                        #T))
    (let ((x (receive*)))
      (cond ((list? x)
             (when (cadr x) ; There is a processor waiting for a ready task.
               (send waiting-processors-mq 'go)))
            (else
             ; Free the task chunk first, otherwise it will be lost and never
             ; freed.
             (send* `(decrement ,task-id))
             (processor-exception x)))))


  (define (activate-ready-task)
    ; Get a ready task from the Ready-Tasks stream and activate it.  For the
    ; waiter, use any value; this tells the next spawn from any processor to
    ; send a message to the waiting-processors message queue when a task is put
    ; in the stream.
    (send* `(stream-get ,ready-tasks-head ,(f 1 #F) #F))
    (let ((x (receive*)))
      (cond ((list? x)
             (let ((x (cadr x)))
               (activate (fv x))
               ; Lose the reference stream-get gives gotten chunks.  This frees
               ; the chunk, because there should not be other references to it.
               (send* `(decrement ,(fv x)))))
            ((eq? 'stream-empty x)
             ; Block until we're told there's a newly-put task or told to
             ; terminate.  A separate global message queue is used, instead of
             ; the processor's normal private queue, to support the 'stop
             ; message (which must not interfere with the synchronization of
             ; other messages, and which must be receivable by all blocked
             ; processors).  Note: It's possible the emulator wants to stop but
             ; the above stream-get still succeeds (because the S.C. continues
             ; servicing requests) and so the 'stop message in the
             ; waiting-processors queue is ignored; this is alright because the
             ; processor process will continue to the instruction-interpreter
             ; where it will notice the SIGTERM delivered to it.
             (case (receive waiting-processors-mq)
               ((go) (activate-ready-task))
               ((stop)
                ; For other possibly blocked processors.
                (send waiting-processors-mq 'stop)
                (exit))
               (else (assert #F))))
            (else (processor-exception x)))))

  ;-----------------------------------------------------------------------------

  (define send*)
  (define receive*)
  (define ready-tasks-head)
  (define ready-tasks-tail)
  (define waiting-processors-mq)

  (define (operations:initialize! sender receiver rt-head rt-tail wp-mq)
    (set! send* sender)
    (set! receive* receiver)
    (set! ready-tasks-head rt-head)
    (set! ready-tasks-tail rt-tail)
    (set! waiting-processors-mq wp-mq))

  ;-----------------------------------------------------------------------------

  (define-logger debug processor-operations (number->string (getpid)))

  ;-----------------------------------------------------------------------------

  (assert (<= (vector-length operations-table) limit-16bit))

)
