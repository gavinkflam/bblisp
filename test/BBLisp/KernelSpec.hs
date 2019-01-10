module BBLisp.KernelSpec
    (
      -- * Spec
      spec
    ) where

import Data.Map ((!))

import qualified BBLisp.Kernel as K
import BBLisp.SyntaxTree (List(..))
import Test.Hspec

-- | Spec for `Kernel`.
spec :: Spec
spec =
    describe "str" $ do
        it "returns the string representation of true" $
            K.str [Boolean True] `shouldBe` Right (String "true")
        it "returns the string representation of false" $
            K.str [Boolean False] `shouldBe` Right (String "false")
        it "returns the string representation of Integer" $
            K.str [Integer 42] `shouldBe` Right (String "42")
        it "returns the string representation of Decimal" $
            K.str [Decimal $ read piStr] `shouldBe` Right (String piStr)
        it "returns the content of String" $
            K.str [String "yolo"] `shouldBe` Right (String "yolo")
        it "returns the name of Symbol" $
            K.str [Symbol "eval"] `shouldBe` Right (String "eval")
        it "returns the string representation of syntactic form" $
            K.str [K.primitives ! "eval"] `shouldBe` Right (String "eval")
        it "returns the string representation of function" $
            K.str [K.primitives ! "str"] `shouldBe` Right (String "str")
        it "returns the concatenation of the string representations" $
            K.str strTestList `shouldBe` Right (String "1world.")
  where
    piStr       = "3.1415926535"
    strTestList = [Integer 1, Symbol "world", String "."]
