Overture
========

Overture is an implementation of the Prelude programming language that
experiments with using ATS as a compiler and verification back end. Prelude
is a synchronous architecture design language for constructing real-time
embedded systems. A prototype compiler for the language was developed by 
Julien Forget as a part of his PhD thesis. This compiler was written in OCaml 
and features a clock calculus that allows a programmer to reason about 
temporal behavior and interaction of communicating tasks in a real time 
system. In addition  to static verification, the compiler generates 
multi-threaded C code that can run inside a real time OS and achieves 
communication between tasks without synchronization primitives.

ATS is a statically typed functional programming language actively developed
at Boston University. It features an advanced type system rooted in the 
Applied Type System logic framework, which gives the language its name. It 
includes both dependent and linear types to statically reason about program 
correctness. We have so far used this type system to capture program 
invariants typically found in system level programs. For example, dependent
types allow us to prove the absence of buffer overflow, and linear types
can ensure programs do not leak memory. In contrast, the Prelude language
deals with temporal invariants in multi-rate periodic systems and are
completely different from any we have tried to capture in ATS previously.

Description
===========

ATS' usefulness goes beyond addressing the typical bugs we find in C programs.
The goal of the Overture project is to demonstrate this by using ATS as a 
foundation for a simple programming language that utilizes ATS as a back end 
for program verification and construction. Put simply, we want to show that 
if you're building a compiler with advanced reasoning features, you do not 
need to start from scratch. Given the mature state of the ATS project and 
its advanced set of features, compiler implementors potentially have a lot 
to work with already. ATS can help you whether your language reasons about 
pointers, buffers, or, in the case of Prelude, time.

Motivation
==========

Prelude stands out as a synchronous programming language by providing
primitives to formally describe temporal behavior of real time tasks.
It combines program construction of a multi-rate periodic system with 
the system's formal specification. It is a domain specific language for 
what we advocate in general programming with ATS. Naturally, we are 
curious to see how the ATS type system compares to Prelude's and what, 
if anything, we can offer to programming languages that aim to improve 
the design of critical real time systems. The correct operation of such 
systems is absolutely vital, and so we argue that embedding more logical 
reasoning into their construction can provide better guarantees of 
fulfilling requirements than just testing alone.

For someone familiar with ATS, capturing the notion of a strictly periodic
clock attributed to a flow is somewhat trivial. Indeed, the type system of
ATS is very feature rich and can capture invariants encompassing multiple
programming paradigms. These features come with quite a steep learning curve,
however, and so a novice may be unable to see when and where they could
be effectively applied. ATS could seem quite attractive for someone aiming
to build a language with much more narrow scope but that aims to unify
program implementation with specification. For people that aspire to build such 
languages, we want to provide ATS as a meta-language to help them quickly
build robust compilers. We hope to facilitate this use case of ATS by doing
an example implementation of Prelude.

From Prelude to ATS
===================

Our recent work where we replaced ATS' default constraint solver with one
based on the Z3 SMT solver will be especially useful in this project. By doing
so, we enriched the statics to be able to understand expressions involving 
new sorts such as arrays, fixed width integers, and rational numbers. Most of
the constraints given in the Prelude programming language involve integer arithmetic
between strictly periodic clocks. The period of these clocks is constricted to
integers as this is required by the scheduling theory used and any generic real time 
operating system. Rational numbers are still used, however, to express the phase
of a periodic task, and this is where our new constraint solver comes in handy.

