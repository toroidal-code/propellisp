;; <Program> -> <Expr>
;;           | (letrec ([lvar <Lambda>] ...) <Expr>)
;;  <Lambda> -> (lambda (var ...) <Expr>)
;;    <Expr> -> <Imm>
;;           | (prim <Expr>)
;;           | (if <Expr> <Expr> <Expr>)
;;           | (and <Expr>* ...)
;;           | (or <Expr>* ...)
;;           | var
;;           | (let ((var <Expr>)* ...) <Expr>)
;;           | (let* ((var <Expr>)* ...) <Expr>)
;;    <Imm>  -> fixnum | boolean | char | null
;;


;;
;;  Constants
;;
(define wordsize 4)

(define fixnum-shift 2)
(define fixnum-mask #x03)
(define fixnum-tag #x00)

(define char-shift 8)
(define char-tag #x0F)
(define char-mask #x3F)

(define boolean-t #x6F)
(define boolean-f #x2F)
(define boolean-bit #x06)
(define boolean-mask #xBF)

(define nil-tag #x3F)


(define fixnum-bits
  (- (* wordsize 8) fixnum-shift))

(define fixnum-lower-bound
  (- (expt 2 (- fixnum-bits 1))))

(define fixnum-upper-bound
  (sub1 (expt 2 (- fixnum-bits 1))))

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fixnum-lower-bound x fixnum-upper-bound)))


;;
;; 1.1 Integers
;; 1.2 Immediate Constants
;;
(define (immediate? expr)
  (or (fixnum? expr) (boolean? expr) (null? expr) (char? expr)))

(define (immediate-rep expr)
  (cond
   ((fixnum? expr) (ash expr fixnum-shift))
   ((boolean? expr) (if expr boolean-t boolean-f))
   ((null? expr) nil-tag)
   ((char? expr) (logor (ash (char->integer expr) char-shift) char-tag))
   (else #f)))

(define unique-constant-label
  (let ((count 0))
    (lambda ()
      (let ([label (format ".LC~s" count)])
  (set! count (add1 count))
  label))))


(define (emit-immediate expr)
  (unless (immediate? expr) (error 'emit-program "value must be an immediate"))
  (let ([rep (immediate-rep expr)])
    (cond
     ((and (< rep 511)  (>= rep 0)) (emit " mov r0, #~a" rep))     ;; 0 <= x < 511
     ((and (> rep -512) (< rep 0)) (emit "  neg r0, #~a" (abs rep)))     ;; -512 < x < 0
     (else  
      (if cog
          (let ([const-label (unique-constant-label)])
            (emit     " mov r0, ~a" const-label)
            (emit-end " .balign 4")
            (emit-end "~a" const-label)
            (emit-end " long  ~a" rep))
          (emit " mvi r0, #~a" rep))))))  ;; all others


;;
;;  1.3 Unary Primitives
;;
(define-syntax define-primitive
  (syntax-rules ()
    ((_ (prim-name si env arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count* (length '(arg* ...)))
       (putprop 'prim-name '*emitter* (lambda (si env arg* ...) b b* ...))))))

(define (primitive? expr)
  (and (symbol? expr) (getprop expr '*is-prim*)))

(define (primitive-call? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (primitive-arg-count sym)
  (or (getprop sym '*arg-count*)
      (error 'primitive-arg-count (format "primitive ~s has no arg count" sym))))

(define (primitive-emitter sym)
  (or (getprop sym '*emitter*)
      (error 'primitive-emitter (format "primitive ~s has no emitter" sym))))

(define (check-primitive-call-args sym args)
  (= (primitive-arg-count sym) (length args)))

(define (emit-primitive-call si env expr)
  (let ([prim (car expr)] 
        [args (cdr expr)])
    (check-primitive-call-args prim args)
    (apply (primitive-emitter prim) si env args)))

(define (emit-boolean-transform . args)
  (let ([comparison (if (null? args) 'IF_E (car args))]
        [inverse    'IF_NE])
    (case comparison
      [(IF_E)  (set! inverse 'IF_NE)] 
      [(IF_B)  (set! inverse 'IF_AE)]
      [(IF_A)  (set! inverse 'IF_BE)]
      [(IF_AE) (set! inverse 'IF_B) ]
      [(IF_BE) (set! inverse 'IF_A) ]
      [else     (set! comparison 'IF_E) (set! inverse 'IF_NE)])
    (emit " ~a  mov r0, #~s" comparison boolean-t)
    (emit " ~a  mov r0, #~s" inverse boolean-f)))

(define (mask-primitive primitive label)
  (putprop label '*is-prim* #t)
  (putprop label '*arg-count* (primitive-arg-count primitive))
  (putprop label '*emitter* (primitive-emitter primitive)))

(define-primitive ($fxadd1 si env arg)
  (emit-expr si env arg)
  (emit " add r0, #~s" (immediate-rep 1)))

(define-primitive ($fxsub1 si env arg)
  (emit-expr si env arg)
  (emit " sub r0, #~s" (immediate-rep 1)))

(define-primitive ($fixnum->char si env arg)
  (emit-expr si env arg)
  (emit " shl r0, #~s" (- char-shift fixnum-shift))
  (emit " or  r0, #~s" char-tag))

(define-primitive ($char->fixnum si env arg)
  (emit-expr si env arg)
  (emit " shr r0, #~s" (- char-shift fixnum-shift)))

(define-primitive ($fxlognot si env arg)
  (emit-expr si env arg)
  (emit " shr r0, #~s" fixnum-shift)
  (emit " xor r0, __MASK_FFFFFFFF") ;; not operator
  (emit " shl r0, #~s" fixnum-shift))

(define-primitive ($fxzero? si env arg)
  (emit-expr si env arg)
  (emit " cmp r0, #~s wz" fixnum-tag)
  (emit-boolean-transform))

(map mask-primitive
     '($fxadd1 $fxsub1 $fixnum->char $char->fixnum $fxlognot $fxzero?)
     '( fxadd1  fxsub1  fixnum->char  char->fixnum  fxlognot  fxzero?))

(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
  (emit " and r0, #~s" fixnum-mask)
  (emit " cmp r0, #~s wz" fixnum-tag)
  (emit-boolean-transform))

(define-primitive (null? si env arg)
  (emit-expr si env arg)
  (emit " cmp r0, #~s wz" nil-tag)
  (emit-boolean-transform))

(define-primitive (boolean? si env arg)
  (emit-expr si env arg)
  (emit " and r0, #~s" boolean-mask)
  (emit " cmp r0, #~s wz" boolean-f)
  (emit-boolean-transform))

(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (emit " and r0, #~s" char-mask)
  (emit " cmp r0, #~s wz" char-tag)
  (emit-boolean-transform))

(define-primitive (not si env arg)
  (emit-expr si env arg)
  (emit " cmp r0, #~s wz" boolean-f)
  (emit-boolean-transform))


;;
;;  1.4 Conditional Expressions
;;

(define (mem-jump)
  (if cog
  "jmp"
  "brw"))

(define unique-label
  (let ((count 0))
    (lambda ()
      (let ([label (format ".L~s" count)])
  (set! count (add1 count))
  label))))

(define (list-expr? sym expr)
  (and (list? expr) (not (null? expr)) (eq? sym (car expr))))

(define (emit-jmp label)
  (emit " ~a #~a" (mem-jump) label))

(define (if? expr)
  (and (list-expr? 'if expr) (= 4 (length expr))))

(define if-predicate  cadr)
(define if-consequent caddr)
(define if-alternate cadddr)

(define (emit-if si env expr)
  (let ([alternate-label (unique-label)]
        [terminal-label (unique-label)])
    (emit-expr si env (if-predicate expr))
    (emit " cmp r0, #~s wz" boolean-f)
    (emit " IF_E ~a #~a" (mem-jump) alternate-label)
    (emit-expr si env (if-consequent expr))
    (emit-jmp terminal-label)
    (emit-label alternate-label)
    (emit-expr si env (if-alternate expr))
    (emit-label terminal-label)))

(define (emit-jump-block si env expr condition label)
  (let ([head (car expr)] 
  [rest (cdr expr)])
    (emit-expr si env head)
    (emit " cmp r0, #~s wz" boolean-f)
    (emit " ~a ~a #~a" condition (mem-jump) label)
    (unless (null? rest)
      (emit-jump-block si env rest condition label))))

(define (emit-conditional-block default condition)
  (lambda (si env expr)
    (case (length expr)
      [(1) (emit-immediate default)]
      [(2) (emit-expr si env (cadr expr))]
      [else
       (let ((end-label (unique-label)))
   (emit-jump-block si env (cdr expr) condition end-label)
   (emit "~a" end-label))])))

(define (and? expr)
  (list-expr? 'and expr))

(define emit-and
  (emit-conditional-block #t "IF_E"))

(define (or? expr)
  (list-expr? 'or expr))

(define emit-or
  (emit-conditional-block #f "IF_NE"))

;;
;; 1.5 Binary Primitives
;;
;; TODO: cog-mode/speed improvements: use general registers r0-r14 by default before moving to stack

; (define next-available-register
;   (let ([count 1])
;   (lambda (register)
;     (if ()))))

; (define unique-constant-label
;   (let ((count 0))
;     (lambda ()
;       (let ([label (format ".LC~s" count)])
;  (set! count (add1 count))
;  label))))

(define (next-stack-index si)
  (- si wordsize))

(define (prev-stack-index si)
  (+ si wordsize))

(define (emit-stack-load-to si register)
  (emit " mov r14, sp")               ;; move the stack pointer to our scratch area
  (emit " sub r14, #~s" (abs si))     ;; 'increment' the stack pointer 'down' a single wordlength
  (emit " rdlong  ~s, r14" register))

(define (emit-stack-save-from si register)
  (emit " mov r14, sp")               ;; move the stack pointer to our scratch area
  (emit " sub r14, #~s" (abs si))     ;; 'increment' the stack pointer 'down' a single wordlength
  (emit " wrlong  ~s, r14" register))

(define (emit-stack-save si)
  (emit-stack-save-from si 'r0))

(define (emit-stack-load si)
  (emit-stack-load-to si 'r0))

(define (emit-binary-operator si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-stack-save si)                  ;; places arg1 value into r0
  (emit-expr (next-stack-index si) env arg2)                
  (emit-stack-load-to si 'r1))          ;; arg0 value to r1

(define (define-binary-predicate op si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  ;;cmps is used, as this is the first case where we might deal with 
  ;;two signed values that _should_ not equal each other
  (emit " cmps  r1, r0 wz, wc") ;; since r1 is our first arg, we use that as the basis for comparison
  (emit-boolean-transform op))

(define-primitive (fx+ si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  (emit " add r0, r1"))        ;; adds r1 to r0, replacing r0

(define-primitive (fx- si env arg1 arg2)
  "Because subtraction is not commutative,
   we have to swap r0 and r1 at the end,
   so that the result is in r0"
  (emit-binary-operator si env arg1 arg2)
  (emit " sub r1, r0")          ;; subtracts r0 (the second arg) from r1 (the first arg), replacing r1
  (emit " mov r0, r1"))          ;; move r1 to r0

(define-primitive (fx* si env arg1 arg2)
  (let ([label (unique-label)])
    (emit-binary-operator si env arg1 arg2)
    (emit " shr r0, #~a" fixnum-shift)
    (emit " mov r2, r0") 
    (emit " min r2, r1") 
    (emit " max r1, r0") 
    (emit " mov r0, #0") 
    (emit "_~a" label) 
    (emit " shr r1, #1  wz, wc") 
    (emit " IF_C  add r0, r2") 
    (emit " add r2, r2") 
    (emit " IF_NZ ~a  #_~a" (mem-jump) label)))

(define-primitive (fxlognot si env arg1)
  (emit-expr si env arg1)
  (emit " shr r0, #~s" fixnum-shift)
  (emit " xor r0, __MASK_FFFFFFFF")
  (emit " shl r0, #~s" fixnum-shift)) ;; not operator

(define-primitive (fxlogor si env arg1 arg2)
  "Since OR is commutative, the ending 
   register doesn't matter, so we use r0"
  (emit-binary-operator si env arg1 arg2)
  (emit " or  r0, r1"))

(define-primitive (fxlogand si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  (emit " and r0, r1"))


(define-primitive (fx= si env arg1 arg2)
  (define-binary-predicate 'IF_E si env arg1 arg2))

(define-primitive (fx< si env arg1 arg2)
  (define-binary-predicate 'IF_B si env arg1 arg2))

(define-primitive (fx<= si env arg1 arg2)
  (define-binary-predicate 'IF_BE si env arg1 arg2))

(define-primitive (fx> si env arg1 arg2)
  (define-binary-predicate 'IF_A si env arg1 arg2))

(define-primitive (fx>= si env arg1 arg2)
  (define-binary-predicate 'IF_AE si env arg1 arg2))


;;
;; 1.6 Local Variables
;;

(define variable? symbol?)

(define (emit-variable-ref env expr)
  (let ([table-entry (assoc expr env)])
    (if table-entry (emit-stack-load (cdr table-entry))
        (error 'emit-variable-ref (format "Undefined variable: ~s" expr)))))

(define (let? expr)
  (list-expr? 'let expr))

(define (let*? expr)
  (list-expr? 'let* expr))

(define (let!? expr)
  (or (let?  expr)
      (let*? expr)))

(define empty? null?)

(define let-bindings cadr)
(define let-body caddr)

(define (extend-env var si new-env)
  (cons (cons var si) new-env))

(define (emit-let si env expr)
  (define (process-let bindings si new-env)
    (cond
     [(empty? bindings) (emit-expr si new-env (let-body expr))]
     (else
      (let ([binding (car bindings)])
        (emit-expr si (if (let*? expr) new-env env) (cadr binding))
        (emit-stack-save si)
        (process-let (cdr bindings)
                     (next-stack-index si)
                     (extend-env (car binding) si new-env))))))
  (process-let (let-bindings expr) si env))

;;
;; 1.7 Procedures
;;

(define (emit-function-header label)
  (emit " .balign 4")
  (emit " .global ~a" label)
  (emit-label label))

(define (emit-call label)
  (if cog
      (emit " jmpret  lr, #~a" label)
      (emit " lcall #~a" label)))

(define  (emit-ret)
  (if cog
      (emit " jmp lr")
      (emit " mov pc,lr")))

(define (letrec? expr)
  (and (list? expr) (not (null? expr)) (eq? (car expr) 'letrec)))

(define (make-initial-env lvars labels)
  (map cons lvars labels))

(define call-target car)

(define (app? expr env)
  (and (list? expr) (not (null? expr)) (assoc (call-target expr) env)))

(define letrec-bindings cadr)
(define letrec-body caddr)

(define unique-labels
  (let ([count  0])
    (lambda (lvars)
      (map (lambda (lvar)
             (let ([label (format "_~s_~s" lvar count)])
               (set! count (add1 count))
               label))
           lvars))))

(define lambda-formals cadr)
(define lambda-body caddr)

(define call-args cdr)

(define (emit-adjust-base si)
  (unless (= si 0)
          (cond ((> 0 si) (emit " sub sp, #~s" (abs si)))
                ((< 0 si) (emit " add sp, #~s" si)))))

(define (emit-app si env expr)
  (define (emit-arguments si args)
    (unless (empty? args)
            (emit-expr si env (car args))
            (emit-stack-save si)
            (emit-arguments (- si wordsize) (cdr args))))
  (emit-arguments (- si wordsize) (call-args expr))
  (emit-stack-save-from si 'lr) 
  (emit-adjust-base si)
  (emit-call (cdr (assoc (call-target expr) env)))
  (emit-adjust-base (- si))
  (emit-stack-load-to si 'lr))

(define (emit-lambda env)
  (lambda (expr label)
    (emit-function-header label)
    (let ([fmls (lambda-formals expr)]
          [body (lambda-body expr)])
      (let f ([fmls fmls]
              [si (- wordsize)]
              [env env])
        (cond
         [(empty? fmls) (emit-expr si env body) (emit-ret)]
         (else
          (f (cdr fmls)
             (- si wordsize)
             (extend-env (car fmls) si env))))))))


(define (emit-letrec expr si)
  (let* ([bindings (letrec-bindings expr)]
         [lvars (map car bindings)]
         [lambdas (map cadr bindings)]
         [labels (unique-labels lvars)]
         [env (make-initial-env lvars labels)])
    (for-each (emit-lambda env) lambdas labels)
    (emit-scheme-entry (letrec-body expr) si env)))

;;
;;  Compiler
;;

(define (emit-label f)
  (emit "~a" f))


(define (emit-expr si env expr)
  (cond
   [(immediate? expr)       (emit-immediate expr)]
   [(if? expr)              (emit-if si env expr)]
   [(and? expr)             (emit-and si env expr)]
   [(or? expr)              (emit-or si env expr)]
   [(primitive-call? expr)  (emit-primitive-call si env expr)]
   [(variable? expr)        (emit-variable-ref env expr)]
   [(let? expr)             (emit-let si env expr)]
   [(let*? expr)            (emit-let si env expr)]
   [(app? expr env)         (emit-app si env expr)]
   (else (error 'emit-expr (format "~s is not a valid expression" expr)))))


(define cog #f)

(define (emit-program-header)
  (emit " .text")
  (emit-function-header "_scheme_entry")
  (emit " sub sp, #4")
  (emit " wrlong  lr, sp")
  (emit-call "_scheme_entry_2")
  (emit " rdlong  lr, sp")
  (emit " add sp, #4")
  (emit-ret))

(define (emit-scheme-entry expr si env)
  (emit-function-header "_scheme_entry_2")
  (emit-expr (- si wordsize) env expr)
  (emit-ret))

(define (emit-cog-program program)
  (set! cog #t)
  (emit-program-header)
  (if (letrec? program) 
      (emit-letrec program (- wordsize))
      (emit-scheme-entry program (- wordsize) (make-initial-env '() '()))))

(define (emit-program program)
  (set! cog #f)
  (emit-program-header)
  (if (letrec? program) 
      (emit-letrec program (- wordsize))
      (emit-scheme-entry program (- wordsize) (make-initial-env '() '()))))
