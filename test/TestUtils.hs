module TestUtils where

import TestConstraint
import Typiara.TypeTree (TypeTree, mergeAt, singleton, triple)
import Typiara.Utils (fromRight)

-- | Unlinked tree representing a function.
fun :: [TestConstraint] -> TypeTree TestConstraint
fun [t] = singleton t
fun [arg, ret] = triple FunConstraint arg ret
fun (t:ts) =
  let base = triple FunConstraint t AnyConstraint
      ret = fun ts
   in fromRight $ mergeAt base [1] ret

-- | Unlinked tree built purely from `ConstraintId`s.
idFun :: [Char] -> TypeTree TestConstraint
idFun = fun . map (ConstraintId . cons)
  where
    cons = (: [])
