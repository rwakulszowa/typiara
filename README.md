# typiara

A library defining a set of generic abstractions useful for implementing type checking logic.
Implements a generic type system based on [Hindley Millner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system), known for its type inference capabilities.

The main entry point to the library is the `Typ` typeclass. If you provide an implementation of `Typ`, you will get a type system for it.
See the test/Examples/ folder for a few samples.

## Code style

Whatever [hindent](https://github.com/mihaimaruseac/hindent), [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) and [hlint](https://github.com/ndmitchell/hlint) told me to do.

## Performance

The library is in an early stage where performance is not the top priority.
Wherever applicable, the code is written in a way that sacrifices performance for correctness.

The implementation is expected to change over time, once the high level interface becomes more stable.

## Why

Because I felt like learning Haskell.
