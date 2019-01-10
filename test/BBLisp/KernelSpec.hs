module BBLisp.KernelSpec
    (
      -- * Spec
      spec
    ) where

import qualified BBLisp.Kernel as K
import BBLisp.SyntaxTree (List(..))
import Test.Hspec

-- | Spec for `Kernel`.
spec :: Spec
spec =
    describe "str" $ do
        it "returns the string representation of Boolean of true" $
            K.str [strTestBoolT] `shouldBe` Right (String "true")
        it "returns the string representation of Boolean of false" $
            K.str [strTestBoolF] `shouldBe` Right (String "false")
        it "returns the string representation of Integer" $
            K.str [strTestInt] `shouldBe` Right (String "42")
        it "returns the string representation of String" $
            K.str [strTestString] `shouldBe` Right strTestString
  where
    strTestBoolT  = Boolean True
    strTestBoolF  = Boolean False
    strTestInt    = Integer 42
    strTestString = String "yolo"
