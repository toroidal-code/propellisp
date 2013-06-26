;; <Program> -> <Expr>
;;           | (letrec ([lvar <Lambda>] ...) <Expr>)
;;  <Lambda> -> (lambda (var ...) <Expr>)
;;    <Expr> -> <Imm>
;;	         | (prim <Expr>)
;;	         | (if <Expr> <Expr> <Expr>)
;;	         | (and <Expr>* ...)
;;	         | (or <Expr>* ...)
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

(define nil #x3F)


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
(define (immediate? x)
  (or (fixnum? x) (boolean? x) (null? x) (char? x)))

(define (immediate-rep x)
  (cond
   ((fixnum? x) (ash x fixnum-shift))
   ((boolean? x) (if x boolean-t boolean-f))
   ((null? x) nil)
   ((char? x) (logor (ash (char->integer x) char-shift) char-tag))
   (else #f)))

(define unique-constant-label
  (let ((count 0))
    (lambda ()
      (let ([label (format ".LC~s" count)])
	(set! count (add1 count))
	label))))


(define (emit-immediate x)
  (unless (immediate? x) (error 'emit-program "value must be an immediate"))
  (let ([rep (immediate-rep x)])
    (cond
     ((and (< rep 511)  (>= rep 0)) (emit "	mov	r0, #~a" rep))	   ;; 0 <= x < 511
     ((and (> rep -512) (< rep 0)) (emit "	neg	r0, #~a" (abs rep)))     ;; -512 < x < 0
     (else  
      (if cog
          (let ([const-label (unique-constant-label)])
            (emit     "	mov	r0, ~a" const-label)
            (emit-end "	.balign 4")
            (emit-end "~a" const-label)
            (emit-end "	long	~a" rep))
          (emit "	mvi	r0, #~a" rep))))))	;; all others


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

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*)
      (error 'primitive-emitter (format "primitive ~s has no emitter" x))))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (primitive-arg-count x)
  (or (getprop x '*arg-count*)
      (error 'primitive-arg-count (format "primitive ~s has no arg count" x))))

(define (check-primcall-args prim args)
  (= (getprop prim '*arg-count*) (length args)))

(define (emit-primcall si env expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si env args)))

(define (emit-predicate . args)
  (let ([comparison (if (null? args) 'IF_E (car args))]
        [inverse    'IF_NE])
    (case comparison
      [(IF_E)  (set! inverse 'IF_NE)] 
      [(IF_B)  (set! inverse 'IF_AE)]
      [(IF_A)  (set! inverse 'IF_BE)]
      [(IF_AE) (set! inverse 'IF_B) ]
      [(IF_BE) (set! inverse 'IF_A) ]
      [else     (set! comparison 'IF_E) (set! inverse 'IF_NE)])
    (emit " ~a	mov	r0, #~s" comparison boolean-t)
    (emit " ~a	mov	r0, #~s" inverse boolean-f)))

(define-primitive ($fxadd1 si env arg)
  (emit-expr si env arg)
  (emit "	add	r0, #~s" (immediate-rep 1)))

(define-primitive ($fxsub1 si env arg)
  (emit-expr si env arg)
  (emit "	sub	r0, #~s" (immediate-rep 1)))

(define-primitive ($fixnum->char si env arg)
  (emit-expr si env arg)
  (emit "	shl	r0, #~s" (- char-shift fixnum-shift))
  (emit "	or 	r0, #~s" char-tag))

(define-primitive ($char->fixnum si env arg)
  (emit-expr si env arg)
  (emit "	shr	r0,	#~s" (- char-shift fixnum-shift)))

(define-primitive ($fxlognot si env arg)
  (emit-expr si env arg)
  (emit "	shr	r0, #~s" fixnum-shift)
  (emit "	xor	r0, __MASK_FFFFFFFF") ;; not operator
  (emit "	shl	r0, #~s" fixnum-shift))

(define-primitive ($fxzero? si env arg)
  (emit-expr si env arg)
  (emit "	cmp	r0, #~s wz" fixnum-tag)
  (emit-predicate))

(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
  (emit "	and	r0, #~s" fixnum-mask)
  (emit "	cmp	r0, #~s wz" fixnum-tag)
  (emit-predicate))

(define-primitive (null? si env arg)
  (emit-expr si env arg)
  (emit "	cmp	r0, #~s wz" nil)
  (emit-predicate))

(define-primitive (boolean? si env arg)
  (emit-expr si env arg)
  (emit "	and	r0, #~s" boolean-mask)
  (emit "	cmp	r0, #~s wz" boolean-f)
  (emit-predicate))

(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (emit "	and	r0, #~s" char-mask)
  (emit "	cmp	r0, #~s wz" char-tag)
  (emit-predicate))

(define-primitive (not si env arg)
  (emit-expr si env arg)
  (emit "	cmp	r0, #~s wz" boolean-f)
  (emit-predicate))


;;
;;  1.4 Conditional Expressions
;;

(define (mem-jump)
	(if cog
	"jmp"
	"brs"))

(define unique-label
  (let ((count 0))
    (lambda ()
      (let ([label (format ".L~s" count)])
	(set! count (add1 count))
	label))))

(define (if? expr)
  (and (list? expr) (eq? (car expr) 'if) (= 4 (length expr))))

(define (if-test expr)
  (cadr expr))

(define (if-conseq expr)
  (caddr expr))

(define (if-altern expr)
  (cadddr expr))

(define (emit-if si env expr)
  (let ([alt-label (unique-label)]
	[end-label (unique-label)])
    (emit-expr si env (if-test expr))
    (emit "	cmp	r0, #~s wz" boolean-f)
    (emit "	IF_E ~a #~a" (mem-jump) alt-label)
    (emit-expr si env (if-conseq expr))
    (emit "	~a #~a" (mem-jump) end-label)
    (emit "~a" alt-label)
    (emit-expr si env (if-altern expr))
    (emit "~a" end-label)))

(define (emit-jump-block si env expr condition label)
  (let ([head (car expr)] 
	[rest (cdr expr)])
    (emit-expr si env head)
    (emit "	cmp	r0, #~s wz" boolean-f)
    (emit "	~a ~a #~a" condition (mem-jump) label)
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
  (and (list? expr) (eq? (car expr) 'and)))

(define emit-and
  (emit-conditional-block #t "IF_E"))

(define (or? expr)
  (and (list? expr) (eq? (car expr) 'or)))

(define emit-or
  (emit-conditional-block #f "IF_NE"))

;;
;; 1.5 Binary Primitives
;;
;; TODO: cog-mode/speed improvements: use general registers r0-r14 by default before moving to stack

; (define next-available-register
; 	(let ([count 1])
; 	(lambda (register)
; 		(if ()))))

; (define unique-constant-label
;   (let ((count 0))
;     (lambda ()
;       (let ([label (format ".LC~s" count)])
;	 (set! count (add1 count))
;	 label))))

(define (emit-stack-load-to si register)
  (emit "	mov	r14, sp")               ;; move the stack pointer to our scratch area
  (emit "	sub	r14, #~s" (abs si))     ;; 'increment' the stack pointer 'down' a single wordlength
  (emit "	rdlong	~s, r14" register))

(define (emit-stack-save-from si register)
  (emit "	mov	r14, sp")               ;; move the stack pointer to our scratch area
  (emit "	sub	r14, #~s" (abs si))     ;; 'increment' the stack pointer 'down' a single wordlength
  (emit "	wrlong	~s, r14" register))

(define (emit-stack-save si)
  (emit-stack-save-from si 'r0))

(define (emit-stack-load si)
  (emit-stack-load-to si 'r0))

(define (emit-binary-operator si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-stack-save si)           ;; places arg1 value into r0
  (emit-expr (- si wordsize) env arg2)                
  (emit-stack-load-to si 'r1))          ;; arg0 value to r16

(define-primitive (fx+ si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  (emit "	add	r0, r1"))        ;; adds r1 to r0, replacing r0

(define-primitive (fx- si env arg1 arg2)
  "Because subtraction is not commutative,
   we have to swap r0 and r1 at the end,
   so that the result is in r0"
  (emit-binary-operator si env arg1 arg2)
  (emit "	sub	r1, r0")          ;; subtracts r0 (the second arg) from r1 (the first arg), replacing r1
  (emit "	mov	r0, r1"))          ;; move r1 to r0

(define-primitive (fx* si env arg1 arg2)
  (let ([label (unique-label)])
    (emit-binary-operator si env arg1 arg2)
    (emit "	shr	r0, #~a" fixnum-shift)
    (emit "	mov	r2, r0") 
    (emit "	min	r2, r1") 
    (emit "	max	r1, r0") 
    (emit "	mov	r0, #0") 
    (emit "_~a" label) 
    (emit "	shr	r1, #1	wz, wc") 
    (emit " IF_C	add	r0, r2") 
    (emit "	add	r2, r2") 
    (emit " IF_NZ	~a	#_~a" (mem-jump) label)))

(define-primitive (fxlognot si env arg1)
  (emit-expr si env arg1)
  (emit "	shr	r0, #~s" fixnum-shift)
  (emit "	xor	r0, __MASK_FFFFFFFF")
  (emit "	shl	r0, #~s" fixnum-shift)) ;; not operator

(define-primitive (fxlogor si env arg1 arg2)
  "Since OR is commutative, the ending 
   register doesn't matter, so we use r0"
  (emit-binary-operator si env arg1 arg2)
  (emit "	or	r0, r1"))

(define-primitive (fxlogand si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  (emit "	and	r0, r1"))

(define (define-binary-predicate op si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  ;;cmps is used, as this is the first case where we might deal with 
  ;;two signed values that _should_ not equal each other
  (emit "	cmps	r1, r0 wz, wc") ;; since r1 is our first arg, we use that as the basis for comparison
  (emit-predicate op))

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
  (let ([pair (assoc expr env)])
    (if pair (emit-stack-load (cdr pair))
        (error 'emit-variable-ref (format "Undefined variable: ~s" expr)))))

(define (let? expr)
  (and (list? expr)
       (eq? (car expr) 'let)
       (= (length expr) 3)))

(define let-bindings cadr)
(define let-body caddr)

(define empty? null?)

(define (next-stack-index si) 
  (- si wordsize))

(define (extend-env var si new-env)
  (cons (cons var si) new-env))

(define (emit-let si env expr)
  (define (process-let bindings si new-env)
    (cond
     ((empty? bindings) (emit-expr si new-env (let-body expr)))
     (else
      (let ([b (car bindings)])
        (format "~a" env)
        (emit-expr si (if (let*? expr) new-env env) (cadr b))
        (emit-stack-save si)
        (process-let (cdr bindings)
                     (next-stack-index si)
                     (extend-env (car b) si new-env))))))
  (process-let (let-bindings expr) si env))


;;
;; 1.6 Supplemental -- let*
;;
(define (let*? expr)
  (and (list? expr) (eq? (car expr) 'let*) (= 3 (length expr))))

(define (emit-let* si env expr)
  (define (process-let bindings si new-env)
    (cond
     ((empty? bindings)
      (emit-expr si new-env (let-body expr)))
     (else
      (let ((b (car bindings)))
        (emit-expr si new-env (cadr b))
        (emit-stack-save si)
        (process-let (cdr bindings)
                     (next-stack-index si)
                     (extend-env (car b) si new-env))))))
  (process-let (let-bindings expr) si env))

;;
;; 1.7 Procedures
;;
(define (emit-call label)
  (if cog
      (emit "	jmpret	lr, #_~a" label)
      (emit "	lcall	#_~a" label)))

(define  (emit-ret)
  (if cog
      (emit "	jmp	lr")
      (emit "	mov	pc,lr")))

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
             (let ([label (format "~s_~s" lvar count)])
               (set! count (add1 count))
               label))
           lvars))))

(define lambda-formals cadr)
(define lambda-body caddr)

(define call-args cdr)

(define (emit-adjust-base si)
  (unless (= si 0)
          (cond ((> 0 si) (emit "	sub	sp, #~s" (abs si)))
                ((< 0 si) (emit "	add	sp, #~s" si)))))

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

(define (mask-primitive prim label)
  (when (primitive? prim)
        (putprop label '*is-prim* #t)
        (putprop label '*arg-count* (primitive-arg-count prim))
        (putprop label '*emitter* (primitive-emitter prim))))

(map mask-primitive
     '($fxzero? $fxsub1)
     '( fxzero?  fxsub1))

;;
;;  Compiler
;;

(define (emit-label f)
  (emit "_~a" f))


(define (emit-expr si env expr)
  (cond
   [(immediate? expr) (emit-immediate expr)]
   [(if? expr)	      (emit-if si env expr)]
   [(and? expr)       (emit-and si env expr)]
   [(or? expr)	      (emit-or si env expr)]
   [(primcall? expr)  (emit-primcall si env expr)]
   [(variable? expr)  (emit-variable-ref env expr)]
   [(let? expr)       (emit-let si env expr)]
   [(let*? expr)      (emit-let si env expr)]
   [(app? expr env)   (emit-app si env expr)]
   (else (error 'emit-expr (format "~s is not a valid expression" expr)))))

(define (emit-function-header f)
  (emit "	.balign 4")
  (emit "	.global _~a" f)
  (emit-label f))

(define (emit-hub-program-footer)
  (emit "	mov	pc, lr"))

(define (emit-function-footer)
  (emit "	jmp lr"))

(define cog #f)

(define (emit-program-header si)
  (emit "	.text")
  (emit-function-header "scheme_entry")
  (emit-stack-save-from si 'lr) 
  (emit-call "scheme_entry_2")
  (emit-stack-load-to si 'lr)
  (if cog
      (emit-function-footer)
      (emit-hub-program-footer)))

(define (emit-scheme-entry expr si env)
  (emit-function-header "scheme_entry_2")
  (emit-expr (- si wordsize) env expr)
  (emit-ret))

(define (emit-cog-program program)
  (set! cog #t)
  (emit-program-header (- wordsize))
  (if (letrec? program) 
      (emit-letrec program (- wordsize))
      (emit-scheme-entry program (- wordsize) (make-initial-env '() '()))))

(define (emit-program program)
  (set! cog #f)
  (emit-program-header (- wordsize))
  (if (letrec? program) 
      (emit-letrec program (- wordsize))
      (emit-scheme-entry program (- wordsize) (make-initial-env '() '()))))
