# typiara
A library defining a set of generic abstractions useful for implementing type checking logic.

## Code style
Whatever hindent and hlint told me to do.

## Performance
The library is in an early stage where performance is not the top priority.
Wherever applicable, the code is written in a way that sacrifices performance for correctness. 

The implementation is expected to change over time, once the high level becomes more stable.

## Current state
The main reason this library exists is because I felt like learning Haskell. The solution is
overcomplicated and not very performant, but it does the trick.
However, a cleanup effort is in progress anyway. The library will be simplified in the near future.
The only parts that will remain are:
- a type system based on [Hindley Millner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)
- some utilities to make it usable
