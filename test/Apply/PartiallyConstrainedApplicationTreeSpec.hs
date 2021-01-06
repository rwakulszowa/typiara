module Apply.PartiallyConstrainedApplicationTreeSpec
  ( spec
  ) where

import Test.Hspec
import TestConstraint
import TestUtils
import Typiara.Apply.ApplicationTree
import Typiara.Apply.PartiallyConstrainedApplicationTree

spec :: Spec
spec = do
  describe "reduceConstrainedNodes" $ do
    it "(A -> B -> C) A x" $ do
      let tree =
            Application
              (Application
                 (Unapplied . Right . idFun $ "ABC")
                 (Unapplied . Right . idFun $ "A"))
              (Unapplied . Left $ 'x')
      reduceConstrainedNodes tree `shouldBe`
        Right
          (Application
             (Unapplied . Right . idFun $ "BC")
             (Unapplied . Left $ 'x'))
