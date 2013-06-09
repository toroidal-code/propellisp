
(define all-tests '())

(define (compile-program x)
  (emit-program x))

(define (compile-cog-program x)
  (emit-cog-program x))

(define-syntax add-tests-with-string-output
  (syntax-rules (=>)
    [(_ test-name [expr => output-string] ...)
     (set! all-tests
        (cons
           '(test-name [expr string  output-string] ...)
            all-tests))]))

(define (build);; cog)
  ;;(if cog
    ;;(unless (zero? (system "make cog"))
      ;;(error 'make "could not build target"))
    (unless (zero? (system "make hub")) 
      (error 'make "could not build target")));;)

(define (execute)
  (unless (zero? (system "make run | tail -1 > stst.out"))
    (error 'make "produced program exited abnormally")))


(define (build-program expr)
   (run-compile expr)
   (build))

(define (get-string)
  (with-output-to-string
    (lambda ()
      (with-input-from-file "stst.out"
        (lambda ()
          (let f ()
            (let ([c (read-char)])
              (cond
               [(eof-object? c) (void)]
               [else (display c) (f)]))))))))

(define (test-with-string-output test-id expr expected-output) ;; cog)
   (run-compile expr) ;;(if cog (run-cog-compile expr) (run-compile expr))
   (build);; cog)
   (execute)
   (unless (string=? expected-output (get-string))
     (error 'test (format "output mismatch for test ~s, expected ~s, got ~s"
        test-id expected-output (get-string)))))

(define (test-one test-id test)
  (let ([expr (car test)]
        [type (cadr test)]
        [out  (caddr test)])
    (printf "test ~s:~s hub ..." test-id expr)
    (flush-output-port)
    (case type
     [(string) (test-with-string-output test-id expr out)]
     [else (error 'test (format "invalid test type ~s" type))])
    (printf " ok\n")
    ; (printf "test ~s:~s cog ..." test-id expr)
    ; (flush-output-port)
    ; (case type
    ;  [(string) (test-with-string-output test-id expr out #t)]
    ;  [else (error 'test (format "invalid test type ~s" type))])
    ; (printf " ok\n")
    ))

(define (test-all)
  (let f ([i 0] [ls (reverse all-tests)])
    (if (null? ls)
        (printf "passed all ~s tests\n" i)
        (let ([x (car ls)] [ls (cdr ls)])
          (let* ([test-name (car x)]
                 [tests (cdr x)]
                 [n (length tests)])
            (printf "Performing ~a tests ...\n" test-name)
            (let g ([i i] [tests tests])
              (cond
                [(null? tests) (f i ls)]
                [else
                 (test-one i (car tests))
                 (g (add1 i) (cdr tests))])))))))


(define input-filter
  (make-parameter (lambda (x) x)
    (lambda (x)
      (unless (procedure? x)
        (error 'input-filter (format "not a procedure ~s" x)))
      x)))

(define runtime-file
  (make-parameter
    "runtime.c"
    (lambda (fname)
      (unless (string? fname) (error 'runtime-file "not a string" fname))
      fname)))


(define compile-port
  (make-parameter
    (current-output-port)
    (lambda (p)
       (unless (output-port? p)
         (error 'compile-port (format "not an output port ~s" p)))
       p)))

(define compile-end-port
  (make-parameter
   (current-output-port)
   (lambda (p2)
     (unless (output-port? p2)
             (error 'compile-end-port (format "not an output port ~s" p2)))
     p2)))

(define show-compiler-output (make-parameter #f))

(define (run-compile expr)
  (let ([p (open-output-file "build/stst.s" 'replace)])
    (parameterize ([compile-port p])
       (compile-program expr))
    (close-output-port p)))

(define (run-cog-compile expr)
  (let ([p (open-output-file "build/stst.s" 'replace)]
        [p2 (open-output-file "build/stst-end.s" 'replace)])
    (parameterize ([compile-port p]
                   [compile-end-port p2])
       (compile-cog-program expr))
    (close-output-port p)
    (close-output-port p2)
    (system "cat build/stst-end.s >> build/stst.s")))

(define (emit . args)
  (apply fprintf (compile-port) args)
  (newline (compile-port)))

(define (emit-end . args)
  (apply fprintf (compile-end-port) args)
  (newline (compile-end-port)))
