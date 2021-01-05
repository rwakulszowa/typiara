{-# LANGUAGE OverloadedLists #-}

module Apply.FullyConstrainedApplicationTreeSpec
  ( spec
  ) where

import Data.Either (isLeft)
import Test.Hspec
import TestConstraint
import Typiara.Apply.ApplicationTree
import Typiara.Apply.FullyConstrainedApplicationTree
import Typiara.TypeTree (TypeTree, mergeAt, singleton, triple)
import Typiara.Utils (fromRight)

fun :: [TestConstraint] -> TypeTree TestConstraint
fun [t] = singleton t
fun [arg, ret] = triple FunConstraint arg ret
fun (t:ts) =
  let base = triple FunConstraint t AnyConstraint
      ret = fun ts
   in fromRight $ mergeAt base [1] ret

spec :: Spec
spec = do
  describe "reduceConstraints" $ do
    it "singleton" $ do
      let tree = Unapplied (singleton AnyConstraint)
      reduceConstraints tree `shouldBe` Right (singleton AnyConstraint)
    it "A -> A apply A" $ do
      let a = ConstraintId "A"
      let tree = Application (Unapplied $ fun [a, a]) (Unapplied $ singleton a)
      reduceConstraints tree `shouldBe` Right (singleton a)
    it "A -> A apply B" $ do
      let a = ConstraintId "A"
      let b = ConstraintId "B"
      let tree = Application (Unapplied $ fun [a, a]) (Unapplied $ singleton b)
      reduceConstraints tree `shouldSatisfy` isLeft
    it "(A -> B -> A) ((A -> B -> A) A B) B" $ do
      let a = ConstraintId "A"
      let b = ConstraintId "B"
      let aba = fun [a, b, a]
      let tree =
            Application
              (Application
                 (Unapplied aba)
                 (Application
                    (Application (Unapplied aba) (Unapplied $ singleton a))
                    (Unapplied $ singleton b)))
              (Unapplied $ singleton b)
      reduceConstraints tree `shouldBe` Right (singleton a)
