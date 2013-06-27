Propellisp
==========

Scheme for the Propeller 32-bit 8-core MCU.

##What it is
This is a simple (for now) compiler for Scheme that targets the Propeller MCU. The compiler is written in Petite Chez Scheme. Currently, I am following the paper Compilers: A Backend to Frontend and Back to Front Again by Abdulaziz Ghuloum.
You can find a copy of it [here](https://github.com/zellio/incrementum/tree/master/doc), in my friend's repository.  
  
Development is done in a tick-tock model, with a large number of features or functions added, and then time devoted to optimization of speed and space.
Since this is an embedded platform, a great deal of value is placed on the ability to optimize away (or in the case of tail-calls, eliminate) unnecessary calls to save space.

##Why?
Because currently, Propeller development is limited to Spin, PASM (both daunting for beginners), and C/C++ using the new propgcc compiler. I wanted to make something that made Propeller devlopment fun and easy, the way Arduino attempts. Lisp (e.g. Scheme) is very easy for beginners to learn, and provides a great degree of flexibility 

Why the Propeller? I initially tried writing a Scheme compiler for AVR, but was put off by the possible loss of precision due to the 8-bit architecture (16-bit ints become 14-bit fixnums, which can only represent up to 16,384). 
I've got a couple of Propeller boards in various states, and the Propeller chip (DIP or QFP) is only about $8. That, combined with the novel 8-core architecture, 
and the recent surge in popularity towards concurrent Lisps (i.e. Clojure) made me decide that the Propeller would be fun to experiment with. 

##Want to help?
Try testing out the repository a little. I'm going to make a comprehensive list of things need to be done in the wiki. If you feel up to it, try implementing a feature or two. Pull requests are always more than welcome.

If you find a bug, or have a feature you'd like to see, please report it using the issues.

##Prerequisites
[propgcc](https://code.google.com/p/propgcc/) is used for compiling the emitted [assembly](https://code.google.com/p/propgcc/downloads/detail?name=as.pdf&can=2&q=)
and the debug C code together. The makefile assumes that this is in the recommended location,`/opt/parallax/`   
[Petite Chez Scheme](http://www.scheme.com/petitechezscheme.html) is used for the backend and assembly emission process.

##Using it
Clone the repository, `cd` into it, and in a Petite REPL do
``` scheme
> (load "test/scheme/tests-driver.scm")
> (load "src/scheme/compiler.scm")
> (run-compile '(some-scheme-code-here))
> (exit)
```
This creates a stst.s assembly file inside the `build/` directory. Then, run `make run` to compile and run the code.

##Testing
All the automated tests are performed using a method of  
`Compile scheme -> Compile C -> Compile Elf -> Upload to Propeller -> Receive Result -> Assert Equality`  
Tests can be run (with a Propeller attached) using `petite run-tests.scm`

##Completed
- 1.1: Fixnums
- 1.2: Immediate Constants: booleans, chars, and nil/'() 
- 1.3: Unary Primitives (e.g. `(fixnum? 42) `)
- 1.4: Conditional Expressions: if, and, or
- 1.5: Binary Primitives and Operators (e.g. `(fx+ 32 4)`, `(fx> 32 -4)`)
- 1.6: Local Variables (let, let*)
- 1.7: Procedures (letrec)
- 1.8: Iteration and Tail calls
- Support for cog vs hub specialized calls (branch, jump, return)
 
##Currently in progress
- Cog mode support (-mcog) for running a program completely on a cog. [Read more here](https://code.google.com/p/propgcc/wiki/PropGccInDepth#Memory_Models_and_Mixed-Mode_Programming)
- 1.9: Heap and data structures (vector, cons pair, etc)

##Todo 
####(beyond the scope of the paper, in order of increasing difficulty)
- Pin twiddling support with an Arduino-like syntax.
- Branch for the Propeller II. This includes re-vamping the stack architecture to utilise the new built-in stack memory
- Threading using `COGNEW`, `COGINIT`, and the in-built semaphores. This will lead to true concurrent operations, such as large array sorting
- Full support for [R7RS (small)](http://trac.sacrideo.us/wg/raw-attachment/wiki/WikiStart/r7rs-draft-9.pdf)

##License
All code is hereby released under MIT License.

###Special Thanks
Thanks to [Zachary Elliot](https://github.com/zellio), for answering all my questions, giving me tips on construction, and providing an impressive compiler for basing my work on.

