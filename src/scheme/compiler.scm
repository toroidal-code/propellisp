(load "tests-driver.scm")

;;;;
;;
;;  < EXPR > -> fixnum
;;
(define (emit-program x)
	(unless (integer? x) (error 'emit-program "value must be an integer"))
	(emit "        .text")
	(emit "        .balign 4")
	(emit "        .global _scheme_entry")
	(emit "_scheme_entry")
	(emit "        mov r0, #~a" x)
	(emit "        mov pc, lr"))
	;;(emit "        ret"))