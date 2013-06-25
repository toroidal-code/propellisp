;;
;;  <EXPR> -> <Imm>
;;	  | (prim <Expr>)
;;	  | (if <Expr> <Expr> <Expr>)
;;	  | (and <Expr>* ...)
;;	  | (or <Expr>* ...)
;;    | var
;;    | (let ((var <Expr>)* ...) <Expr>)
;;  <Imm>  -> fixnum | boolean | char | null
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

(define (emit-binary-operator si env arg1 arg2)
  (emit-expr si env arg1)
  ;(emit "	sub	sp, #~s" (abs si env ))   ;; 'increment' the stack pointer 'down'
  (emit "	mov	r14, sp")               ;; move the stack pointer to our scratch area
  (emit "	sub	r14, #~s" (abs si))     ;; 'increment' the stack pointer 'down' a single wordlength
  (emit "	wrlong	r0, r14")           ;; moves first arg to the stack
  (emit-expr (- si wordsize) env arg2)   ;; places arg1 value into r0
  (emit "	mov	r14, sp")               ;; move the stack pointer to our scratch area
  (emit "	sub	r14, #~s" (abs si)) 
  (emit "	rdlong	r1, r14"))          ;; arg0 value to r1)

(define (emit-binary-ending) '())
  ;(emit "	add	sp, #~s" (abs si))) ;; 'decrement' the stack pointer 'up'
  ;;(emit "	add	sp, #~s" wordsize))   ;; 'decrement' the stack pointer 'up' a single wordlength

(define-primitive (fx+ si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  (emit "	add	r0, r1")        ;; adds r1 to r0, replacing r0
  (emit-binary-ending))

(define-primitive (fx- si env arg1 arg2)
  "Because subtraction is not commutative,
   we have to swap r0 and r1 at the end,
   so that the result is in r0"
  (emit-binary-operator si env arg1 arg2)
  (emit "	sub	r1, r0")          ;; subtracts r0 (the second arg) from r1 (the first arg), replacing r1
  (emit "	mov	r0, r1")          ;; move r1 to r0
  (emit-binary-ending))

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
    (emit " IF_NZ	~a	#_~a" (mem-jump) label) 
    (emit-binary-ending)))

(define-primitive (fxlognot si env arg1)
	(emit-expr si env arg1)
  (emit "	shr	r0, #~s" fixnum-shift)
  (emit "	xor	r0, __MASK_FFFFFFFF")
  (emit "	shl	r0, #~s" fixnum-shift)) ;; not operator

(define-primitive (fxlogor si env arg1 arg2)
  "Since OR is commutative, the ending 
   register doesn't matter, so we use r0"
  (emit-binary-operator si env arg1 arg2)
  (emit "	or	r0, r1")
  (emit-binary-ending))

(define-primitive (fxlogand si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  (emit "	and	r0, r1")
  (emit-binary-ending))

(define (define-binary-predicate op si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  ;;cmps is used, as this is the first case where we might deal with 
  ;;two signed values that _should_ not equal each other
  (emit "	cmps	r1, r0 wz, wc") ;; since r1 is our first arg, we use that as the basis for comparison
  (emit-predicate op)
  (emit-binary-ending))

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

(define (emit-stack-load si)
  (emit "	mov	r14, sp")               ;; move the stack pointer to our scratch area
  (emit "	sub	r14, #~s" (abs si)) 
  (emit "	rdlong	r0, r14"))

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

(define (emit-stack-save si)
  (emit "	mov	r14, sp")               ;; move the stack pointer to our scratch area
  (emit "	sub	r14, #~s" (abs si)) 
  (emit "	wrlong	r0, r14"))

(define (next-stack-index si) 
  (- si wordsize))

(define (extend-env var si new-env)
  (cons (cons var si) new-env))

(define (emit-let si env expr)
  (define (process-let bindings si new-env)
    (cond
     ([empty? bindings] (emit-expr si new-env (let-body expr)))
     (else
      (let ([b (car bindings)])
        (emit-expr si env (cadr b))
        (emit-stack-save si)
        (process-let (cdr bindings)
                     (next-stack-index si)
                     (extend-env (car b) si new-env))))))
  (process-let (let-bindings expr) si env))





;;
;;  Compiler
;;

(define (emit-label f)
  (emit "_~a" f))


(define (emit-expr si env expr)
  (cond
   ([immediate? expr] (emit-immediate expr))
   ([if? expr]	      (emit-if si env expr))
   ([and? expr]       (emit-and si env expr))
   ([or? expr]	      (emit-or si env expr))
   ([primcall? expr]  (emit-primcall si env expr))
   ([variable? expr]  (emit-variable-ref env expr))
   ([let? expr]       (emit-let si env expr))
   (else (error 'emit-expr (format "~s is not a valid expression" expr)))))

(define (emit-function-header f)
  (emit "	.text")
  (emit "	.balign 4")
  (emit "	.global _~a" f)
  (emit-label f))

; (define (emit-function-footer)
;   (emit "	mov r0, r7"))

(define (emit-hub-function-footer)
  ;;(emit-function-footer)
  (emit "	mov	pc, lr"))

(define (emit-cog-function-footer)
  ;;(emit-function-footer)
  (emit "	jmp lr"))

(define cog #f)

(define (emit-program expr)
  (set! cog #f)
  (emit-function-header "scheme_entry")
  ; (emit "	sub	sp, #~a" wordsize)
  ; (emit "	mov	r7, sp")
  ; (emit "	wrlong	r0, r7")
  ; (emit "	mov	r7, sp")
  ; (emit "	rdlong	r7, r7")
  ; (emit "	~a 	_L_scheme_entry" (mem-jump))
  ; (emit-label "L_scheme_entry")
  (emit-expr (- wordsize) '() expr)
  ; (emit "	add	sp, #~a" wordsize)
  (emit-hub-function-footer))

(define (emit-cog-program expr)
  (set! cog #t)
  (emit-function-header "scheme_entry")
  ; (emit "	sub	sp, #~a" wordsize)
  ; (emit "	mov	r7, sp")
  ; (emit "	wrlong	r0, r7")
  ; (emit "	mov	r7, sp")
  ; (emit "	rdlong	r7, r7")
  ; (emit "	~a 	_L_scheme_entry" (mem-jump))
  ; (emit-label "L_scheme_entry")
  (emit-expr (- wordsize) '() expr)
  ; (emit "	add	sp, #~a" wordsize)
  (emit-cog-function-footer))

