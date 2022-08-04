<!--

Please keep this document correctly word-wrapped at 70 columns and
with no trailing whitespace or blank lines at the end! Merge requests
with modifications to this document will be not be accepted until it
is formatted correctly! ~~abb

-->

# AlexScript

AlexScript (which is a misnomer; the language will soon be renamed
since it's not actually a scripting language) is a programming
language designed to have a functional feel and very high-level
ergonomics while maintaining speed using strictly-controlled,
deterministic memory management. The language is capable of compiling
to C (and possibly other languages in the future), allowing for
maximum portability without having to write a new backend for the
compiler for every possible target; also, the compiler and tooling
will eventually be rewritten in AlexScript to allow for maximum
portability.

## Example

```
// Calculates the nth fibonacci number.
def fibonacci (0: U32) : U32 = 0
  | fibonacci 1 = 1
  | fibonacci n = fibonacci (n - 2) + fibonacci (n - 1);

// Prompts the user for a number n, and prints the Fibonacci numbers up
// to n.
def fibPrompt
  = do {
    print "Enter a number: ";
    num <- read <$> getLine;
    sequence_ (print . fibonacci <$> [0 .. num]);
  };

// Program entry point.
def main: IO ()
  = fibPrompt;
```

Note that type annotations are always optional; here they're given for
`fibonacci` and `main` for illustrative purposes, but omitted for
`fibPrompt` (for which the compiler infers the return type `IO ()`).

## Tools

This repository contains the following tools:
- `axc`, the AlexScript compiler. This can be used as a binary with a
  fairly standard compiler CLI, or as a library for use in other
  programs.

The following tools do not exist yet, but are planned:
- `axci`, the interactive AlexScript interpreter.
- `axcd`, the Language Server Protocol (LSP) server for AlexScript
  code support in editors, supporting definition peeking and lookup,
  renaming variables and modules, etc.
- `axfmt`, the standard formatter for AlexScript code; all AlexScript
  code used in this repository must be formatted with `axfmt`, and its
  use is recommended for other projects.
- `axdoc`, the documentation generator.
- `alexscript-mode`, an Emacs mode for editing AlexScript code,
  supporting syntax highlighting, automatic indentation, some basic
  keybindings for common tasks, Emacs-side LSP integration for
  communicating with `acxd`, and a collection of `yasnippet` snippets
  for inserting common AlexScript constructs.
- `alexscript-vsc`, Visual Studio Code plugins and tools for editing
  AlexScript code.
- `alexscript-vim`, tools and configuration files for optimizing Vim
  and Neovim for editing AlexScript code.

## Language features

The language is mostly influenced by Rust and Haskell: it has strict
safety requirements and borrow-checked memory management like that of
Rust, but its syntax and type system are similar to those of Haskell.

Some features the language will most likely have:
- All functions are pure by default; side effects are chained together
  using an `IO` monad.
- Despite the language's purity, expressions will be strictly
  evaluated to provide more programmer control.
- Different monads represent different levels of safety, and can be
  converted using functions marked as `UNSAFE`. The intention is that
  code can be audited by manually checking that all the `UNSAFE`
  transformations are sound, and code that contains no `UNSAFE`
  function calls are guaranteed to satisfy varying definitions of
  soundness:
  - The `IO` monad represents computations that might have side
    effects on the real world. If a computation of type `IO` is known
    by the programmer to not have side effects on the real world, then
    it can be converted to a pure computation using the standard
    library function `UNSAFE_assertPure : IO a -> a`.
  - The `MemoryUnsafe` monad represents computations that might read
    from or write to memory that is not allocated correctly: for
    example, `readPtr`, which reads from a raw pointer, is of type
    `MemoryUnsafe a` because the pointer is not known to be valid. If
    a computation has been confirmed to be safe by the programmer, it
    can be converted to an `IO` computation using
    `UNSAFE_assertMemorySafe : MemoryUnsafe a -> IO a`.
  - Further safety monads may be added in the future.
- The language achieves good performance by guaranteeing a number of
  optimizations:
  - Since the language uses a linear type system, garbage collection
    is not done; instead, values are stored on the stack unless
    explicitly declared to be on the heap, and heap-stored values are
    cleaned up at deterministic points as calculated at compile time.
  - Functions of type `Fn t -> t` are optimized into functions that
    operate on pointers to `t`, i.e., notionally, `Fn (*mut t) -> ()`,
    where `*mut t` is a mutable pointer to a type `t`.
  - Types that contain an optional, non-null pointer like `Option (Box
    a)`, `Option (Ref a)`, etc., are optimized into nullable pointers.
  - Since the language has no loops, the compiler guarantees
    optimization of tail-call recursion to loops on all functions, as
    is standard in functional languages.

## Compilation

When invoked with no flags, AlexScript by default compiles source code
directly to code that is valid C and C++, then calls the system C
compiler to generate an object file, then calls the system linker to
generate an executable file.
