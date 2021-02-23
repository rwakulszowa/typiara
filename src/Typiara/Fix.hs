module Typiara.Fix where

-- | Fixpoint.
--
-- See https://bartoszmilewski.com/2017/02/28/f-algebras/ for a really
-- interesting explanation.
--
-- TLDR: allows wrapping types around into cycles.
-- A functor `C a` can be turned into a recursive datastructure by `Fix C`,
-- becoming a container of itself. Note, that `a` is lost in the process.
-- Information stored in `a` should be encoded in a wrapper functor on top
-- of `C`.
--
-- TODO: add some examples in a spec file.
newtype Fix f =
  Fix (f (Fix f))

unFix (Fix x) = x
