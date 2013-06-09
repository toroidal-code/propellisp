;;
;;  <EXPR> -> <Imm>
;;          | (prim <Expr>)
;;          | (if <Expr> <Expr> <Expr>)
;;          | (and <Expr>* ...)
;;          | (or <Expr>* ...)
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
     ((and (< rep 511)  (>= rep 0)) (emit "        mov	r7, #~a" rep))           ;; 0 <= x < 511
     ((and (> rep -512) (< rep 0)) (emit "        neg	r7, #~a" (abs rep)))     ;; -512 < x < 0
     (else  
      (if cog
          (let ([const-label (unique-constant-label)])
            (emit     "        mov	r7, ~a" const-label)
            (emit-end "        .balign 4")
            (emit-end "~a" const-label)
            (emit-end "        long	~a" rep))
          (emit "        mvi	r7, #~a" rep))))))        ;; all others


;;
;;  1.3 Unary Primitives
;;
(define-syntax define-primitive
  (syntax-rules ()
    ((_ (prim-name arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count* (length '(arg* ...)))
       (putprop 'prim-name '*emitter* (lambda (arg* ...) b b* ...))))))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*)
      (error 'primitive-emitter (format "primitive ~s has no emitter" x))))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (check-primcall-args prim args)
  (= (getprop prim '*arg-count*) (length args)))

(define (emit-primcall expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) args)))

(define (emit-predicate)
  (emit "        IF_E	mov	r7, #~s" boolean-t)
  (emit "        IF_NE	mov	r7, #~s" boolean-f))

(define-primitive ($fxadd1 arg)
  (emit-expr arg)
  (emit "        add	r7, #~s" (immediate-rep 1)))

(define-primitive ($fxsub1 arg)
  (emit-expr arg)
  (emit "        sub	r7, #~s" (immediate-rep 1)))

(define-primitive ($fixnum->char arg)
  (emit-expr arg)
  (emit "        shl	r7, #~s" (- char-shift fixnum-shift))
  (emit "        or 	r7, #~s" char-tag))

(define-primitive ($char->fixnum arg)
  (emit-expr arg)
  (emit "        shr	r7,	#~s" (- char-shift fixnum-shift)))

(define-primitive ($fxlognot arg)
  (emit-expr arg)
  (emit "        shr	r7, #~s" fixnum-shift)
  (emit "        xor	r7, __MASK_FFFFFFFF") ;; not operator
  (emit "        shl	r7, #~s" fixnum-shift))

(define-primitive ($fxzero? arg)
  (emit-expr arg)
  (emit "        cmp	r7, #~s wz" fixnum-tag)
  (emit-predicate))

(define-primitive (fixnum? arg)
  (emit-expr arg)
  (emit "        and	r7, #~s" fixnum-mask)
  (emit "        cmp	r7, #~s wz" fixnum-tag)
  (emit-predicate))

(define-primitive (null? arg)
  (emit-expr arg)
  (emit "        cmp	r7, #~s wz" nil)
  (emit-predicate))

(define-primitive (boolean? arg)
  (emit-expr arg)
  (emit "        and	r7, #~s" boolean-mask)
  (emit "        cmp	r7, #~s wz" boolean-f)
  (emit-predicate))

(define-primitive (char? arg)
  (emit-expr arg)
  (emit "        and	r7, #~s" char-mask)
  (emit "        cmp	r7, #~s wz" char-tag)
  (emit-predicate))

(define-primitive (not arg)
  (emit-expr arg)
  (emit "        cmp	r7, #~s wz" boolean-f)
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

(define (emit-if expr)
  (let ([alt-label (unique-label)]
        [end-label (unique-label)])
    (emit-expr (if-test expr))
    (emit "        cmp	r7, #~s wz" boolean-f)
    (emit "        IF_E ~a #~a" (mem-jump) alt-label)
    (emit-expr (if-conseq expr))
    (emit "        ~a #~a" (mem-jump) end-label)
    (emit "~a" alt-label)
    (emit-expr (if-altern expr))
    (emit "~a" end-label)))

(define (emit-jump-block expr condition label)
  (let ([head (car expr)] 
        [rest (cdr expr)])
    (emit-expr head)
    (emit "        cmp	r7, #~s wz" boolean-f)
    (emit "        ~a ~a #~a" condition (mem-jump) label)
    (unless (null? rest)
            (emit-jump-block rest condition label))))

(define (emit-conditional-block default condition)
  (lambda (expr)
    (case (length expr)
      [(1) (emit-immediate default)]
      [(2) (emit-expr (cadr expr))]
      [else
       (let ((end-label (unique-label)))
         (emit-jump-block (cdr expr) condition end-label)
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
;;  Compiler
;;
(define (emit-expr expr)
  (cond
   ((immediate? expr) (emit-immediate expr))
   ((if? expr)        (emit-if expr))
   ((and? expr)       (emit-and expr))
   ((or? expr)        (emit-or expr))
   ((primcall? expr)  (emit-primcall expr))
   (else (error 'emit-expr (format "~s is not a valid expression" expr)))))

(define (emit-function-header f)
  (emit "        .text")
  (emit "        .balign 4")
  (emit "        .global _~a" f)
  (emit "_~a" f))


(define (emit-hub-function-header f)
  (emit "        .text")
  (emit "        .balign 4")
  (emit "        .global _~a" f)
  (emit "_~a" f)
  (emit "        sub	sp, #4")
  (emit "        mov	r7, sp")
  (emit "        wrlong	r0, r7")
  (emit "        mov	r7, sp")
  (emit "        rdlong	r7, r7"))


(define (emit-cog-function-header)
  (emit "        .text")
  (emit "        .global _~a" f)
  (emit "_~a" f))

(define (emit-function-footer)
  (emit "        mov	r0, r7"))

(define (emit-hub-function-footer)
;;  (emit "        add	sp, #4")
  (emit-function-footer)
  ;;(emit "        lret")
  (emit "        mov	pc, lr"))

(define (emit-cog-function-footer)
  (emit-function-footer)
  (emit "        jmp lr"))

(define cog #f)

(define (emit-program x)
  (set! cog #f)
  (emit-function-header "scheme_entry")
  (emit-expr x)
  (emit-hub-function-footer))

(define (emit-cog-program x)
  (set! cog #t)
  (emit-function-header "scheme_entry")
  (emit-expr x)
  (emit-cog-function-footer))
