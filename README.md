# Forsp: A Forth+Lisp Hybrid Lambda Calculus Language

An Odin port of Forsp, a hybrid language combining Forth and Lisp.

Forsp is a minimalist language.

## Features

Forsp has:
  - An S-Expression syntax like Lisp
  - Function abstraction like Lisp
  - Function application like Forth
  - An environment structure like Lisp
  - Lexically-scoped closures like Lisp (Scheme)
  - Cons-cells / lists / atoms like Lisp
  - A value/operand stack like Forth
  - An ability to express the Lambda Calculus
  - A Call-By-Push-Value evaluation order
  - Only 3 syntax special forms: ' ^ $
  - Only 1 eval-time special form: quote
  - Only 10 primitive functions need to self-implement
  - Ability to self-implement in very little code

## Port

Since this is a port of the original [Forsp](https://github.com/xorvoid/forsp)
it's suggested to check out the original C version too. This was, more-or-less
a fun few day project to port the C code to Odin. I've also always liked the idea of
Forth. I've implemented a bare metal Forth before, but have never done a Lisp,
and this seemed like a good introduction implementation as the original C is
< 700 lines of code.

Note on memory usage. Like the original, there is currently no memory
management. Basically nothing is cleaned up until the program exits. I'd like
to address this at some point.

Given time & interest, I may rework this from a stand alone program to an
package + program. For this reason, consider the API unstable and subject to
change without any guarantees.


## Discussion

See blog post for details: [Forsp: A Forth+Lisp Hybrid Lambda Calculus Language](https://xorvoid.com/forsp.html)

## Tutorial

The best way to learn Forsp is to [read the tutorial](examples/tutorial.fp)

## Recursive Factorial Example

```
(
  ($x x)                       $force
  (force cswap $_ force)       $if
  ($f $t $c $fn ^f ^t ^c fn)   $endif

  ; Y-Combinator
  ($f
    ($x (^x x) f)
    ($x (^x x) f)
    force
  ) $Y

  ; rec: syntax sugar for applying the Y-Combinator
  ($g (^g Y)) $rec

  ; factorial
  ($self $n
    ^if (^n 0 eq 1)
      (^n 1 - self ^n *)
    endif
  ) rec $factorial

  5 factorial print
)
```

## Self-implementation (compute() and eval() core functions)

```
  ; compute [$comp $stack $env -> $stack]
  ($compute $eval
    ^if (dup is-nil) (rot $_ $_) ( ; false case: return $stack
      stack-pop
      ^if (dup 'quote eq)
        ($_ stack-pop rot swap stack-push swap ^eval compute)
        (swap $comp eval ^comp ^eval compute) endif
    ) endif
  ) rec $compute

  ; eval: [$expr $stack $env -> $stack $env]
  ($eval
    ^if (dup is-atom) (
      over2 swap env-find dup $callable
      ^if (dup is-closure) (swap $stack cdr unpack-closure ^stack swap ^eval compute)
      (^if (dup is-clos)   (force)
                           (stack-push)  endif) endif)
    (^if (dup is-list)
      (over2 swap make-closure stack-push)
      (stack-push) endif) endif
  ) rec $eval
```

## Building and Demo

Building:

```
odin build . -out:forsp
```
If you want the low level primitives needed for
[low-level.fp](examples/low-level.fp) or
[fibonacci-imperative.fp](examples/fibonacci-imperative.fp) compile with
`USE_LOWLEVEL`
```
odin build . -out:forsp -define:USE_LOWLEVEL=true
```

If you want to see memory usage after the program exits build with `-debug`

Examples:

```
./forsp examples/tutorial.fp
./forsp examples/demo.fp
./forsp examples/factorial.fp
./forsp examples/currying.fp
./forsp examples/church-numerals.fp
./forsp examples/higher-order-functions.fp
./forsp examples/church-numerals.fp
./forsp examples/low-level.fp
./forsp examples/fibonacci-imperative.fp
./forsp examples/fibonacci-functional.fp
./forsp examples/forsp.fp
```
