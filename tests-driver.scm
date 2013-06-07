(define (compile-program x)
  (emit-program x))

(define (run-compile expr)
  (let ([p (open-output-file "build/stst.s" 'replace)])
    (compile-program expr p)
    (close-output-port p)))

(define (build)
  (unless (zero? (system "make"))
    (error 'make "could not build target")))
;;  (unless (zero? (system "gcc -o stst startup.c stst.s"))
;;    (error 'make "could not build target")))

(define (execute)
  (unless (zero? (system "./bin/stst > stst.out"))
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

(define show-compiler-output (make-parameter #f))

(define (run-compile expr)
  (let ([p (open-output-file "build/stst.s" 'replace)])
    (parameterize ([compile-port p])
       (compile-program expr))
    (close-output-port p)))


(define (execute)
  (unless (fxzero? (system "./bin/stst > stst.out"))
    (error 'execute "produced program exited abnormally")))

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


(define (emit . args)
  (apply fprintf (compile-port) args)
  (newline (compile-port)))
