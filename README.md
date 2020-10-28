# Stutter

Stutter is a minimal language inspired by Lisp.

## Minimal

By minimal we mean that the Stutter interpreter contains the a minimal amount of functions to create a functional language: the "standard library" is written in Stutter itself.

## Inspired by Lisp

Stutter is inspired by Lisp: it has two main datatypes: S-expressions and F-expressions.

## Understanding Stutter

Stutter has a REPL where you can type in expressions and see the result:
```
stutter> (+ 5 3)
8.0
stutter> + 2 3
5.0
```


### Functions

Stutter has a couple of built-in functions (for example `show`)
```
stutter> show 20
20.0
()
```

Functions are called by putting the function first, followed by the arguments. This is the same for math operators:

```
stutter> + 2 3
5
```

The `show` function prints the first argument to stdout, and returns `()` (an empty S-expression, also called `nil`)

### F-expression


### Fixed expressions

If one wants to postpone the evaluation of an expression to a later point, one uses `[ expr ]` to create a **fixed expression**

```
stutter> [+ 5 3]
[+ 5 3]
```

A fixed expression can be evaluated with `eval`

```
stutter> eval [+ 5 3]
8
```

Note that this feature is totally useless in isolation - but with some more language features, it is indispensable.

### Variables

Stutter has an environment which maps variables onto expressions - `def` inserts such an expression into the environment.
In order to capture the variable name, the first argument of `def` needs an f-expression with one or more variable names:

```
stutter> def [a] 42
()
stutter> a
42.0
stutter> def [b c] 2 3
()
stutter> * b c
6
```


### Defining functions

To create a function:

```
stutter> fun [plus a b] [+ a b]
()
stutter> plus 40 2
42
```

# Getting started

- Install stack `sudo apt install haskell-stack`
- Clone the repository
- Install the dependencies, run `stack install`
- Start the interactive haskell shell: `stack repl`
- In the shell, start the the Stutter-Repl: `main`

