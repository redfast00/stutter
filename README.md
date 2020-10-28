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

Stutter has an F-expression (fixed expression), which allows for delayed evaluation: if we don't want a variable to evaluate, we can use an F-expression:

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

The `def` function defines an F-expression of variables (first argument) to its remaining arguments.


# Getting started

- Install stack `sudo apt install haskell-stack`
- Clone the repository
- Install the dependencies, run `stack install`
- Start the interactive haskell shell: `stack repl`
- In the shell, start the the Stutter-Repl: `main`

