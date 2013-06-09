Propellisp
==========

Scheme for the Propeller 32-bit 8-core MCU.

##What it is
This is a simple (for now) compiler for Scheme that targets the Propeller MCU. The compiler is written in Petite Chez Scheme. Currently, I am following the paper Compilers: A Backend to Frontend and Back to Front Again.

##Why?
Because currently, Propeller development is limited to Spin, PASM (both daunting for beginners), and C/C++ using the new propgcc compiler. I wanted to make something that made Propeller devlopment fun and easy, the way Arduino attempts. Lisp (e.g. Scheme) is very easy for beginners to learn, and provides a great degree of flexibility 

##Prerequisites
[propgcc](https://code.google.com/p/propgcc/) is used for compiling the emitted assembly and the debug C code together. The makefile assumes that this is in the recommended location,`/opt/parallax/`   
[Petite Chez Scheme](http://www.scheme.com/petitechezscheme.html) is used for the backend and assembly emission process.

##Using it
Clone the repository, `cd` into it, and in a Petite REPL do
``` scheme
> (load "test/scheme/tests-driver.scm")
> (load "src/scheme/compiler.scm")
> (run-compile '(some-scheme-code-here))
> (exit)
```
This creates a stst.s assembly file inside build. Then, run `make run` to compile and run the code.

##Testing
All the automated tests are performed using a method of  
`Compile scheme -> Compile C -> Compile Elf -> Upload to Propeller -> Receive Result -> Assert Equality`  
Tests can be run (with a Propeller attached) using `petite run-tests.scm`

##Completed
- 1.1: Fixnums
- 1.2: Immediate Constants: booleans, chars, and nil/'() 
- 1.3: Unary Primitives (e.g. `(fixnum? 42) `)
- 1.4: Conditional Expressions: if, and, or

##Currently in progress
- 1.5: Binary Primitives (e.g. `(fx+ 32 4)`)
- Cog mode support (-mcog) for running a program completely on a cog. [Read more here](https://code.google.com/p/propgcc/wiki/PropGccInDepth#Memory_Models_and_Mixed-Mode_Programming)


##Todo 
####(beyond the scope of the paper)
- Branch for the Propeller II. This includes re-vamping the stack architecture to utilise the new built-in stack memory
- Pin twiddling support with an Arduino-like syntax.
- Threading using `COGNEW`, `COGINIT`, and the in-built semaphores. This will lead to true concurrent operations, such as large array sorting

##License
All code is hereby released under MIT License.

###Special Thanks
Thanks to [Zachary Elliot](https://github.com/zellio), for answering all my questions, giving me tips on construction, and providing an impressive compiler for basing my work on.

