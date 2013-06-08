;;;;
;;
;;  <EXPR> -> <Imm>
;;          | (prim <EXPR)
;;  <Imm>  -> fixnum | boolean | char | null


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Immediate handlers
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

(define (emit-immediate x)
  (unless (immediate? x) (error 'emit-program "value must be an immediate"))
  (emit "        mov	r0, #~a" (immediate-rep x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Primirive Handlers
;;
(define-syntax define-primitive
  (syntax-rules ()
    ((_ (prim-name arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count* (length '(arg* ...)))
       (putprop 'prim-name '*emitter* (lambda (arg* ...) b b* ...))))
    ))

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
  (emit "        IF_E	mov	r0, #~s" boolean-t)
  (emit "        IF_NE	mov	r0, #~s" boolean-f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Primative definitions
;;
(define-primitive ($fxadd1 arg)
  (emit-expr arg)
  (emit "        add	r0, #~s" (immediate-rep 1)))

(define-primitive ($fxsub1 arg)
  (emit-expr arg)
  (emit "        sub	r0, #~s" (immediate-rep 1)))

(define-primitive ($fixnum->char arg)
  (emit-expr arg)
  (emit "        shl	r0, #~s" (- char-shift fixnum-shift))
  (emit "        or		r0, #~s" char-tag))

(define-primitive ($char->fixnum arg)
  (emit-expr arg)
  (emit "        shr	r0,	#~s" (- char-shift fixnum-shift)))

(define-primitive ($fxlognot arg)
  (emit-expr arg)
  (emit "        shr	r0, #~s" fixnum-shift)
  (emit "        xor	r0, __MASK_FFFFFFFF") ;; not operator
  (emit "        shl	r0, #~s" fixnum-shift))

(define-primitive ($fxzero? arg)
  (emit-expr arg)
  (emit "        mov	r0, r0")
  (emit "        and	r0, __MASK_000000FF") ;;get the lower byte
  (emit "        cmp	r0, #~s wz" fixnum-tag)
  (emit-predicate))

(define-primitive (fixnum? arg)
  (emit-expr arg)
  (emit "        and	r0, #~s" fixnum-mask)
  (emit "        cmp	r0, #~s wz" fixnum-tag)
  (emit-predicate))

(define-primitive (null? arg)
  (emit-expr arg)
  (emit "        cmp	r0, #~s wz" nil)
  (emit-predicate))

(define-primitive (boolean? arg)
  (emit-expr arg)
  (emit "        and	r0, #~s" boolean-mask)
  (emit "        cmp	r0, #~s wz" boolean-f)
  (emit-predicate))

(define-primitive (char? arg)
  (emit-expr arg)
  (emit "        and	r0, #~s" char-mask)
  (emit "        cmp	r0, #~s wz" char-tag)
  (emit-predicate))

(define-primitive (not arg)
  (emit-expr arg)
  (emit "        cmp	r0, #~s wz" boolean-f)
  (emit-predicate))


(define (emit-expr expr)
  (cond
   ((immediate? expr) (emit-immediate expr))
   ((primcall? expr) (emit-primcall expr))
   (else (error 'emit-expr (format "~s is not a valid expression" expr)))))

(define (emit-function-header f)
	(emit "        .text")
	(emit "        .balign 4")
	(emit "        .global _~a" f)
	(emit "_~a" f))

(define (emit-program x)
  (emit-function-header "scheme_entry")
  (emit-expr x)
  (emit "        mov	pc,lr"))