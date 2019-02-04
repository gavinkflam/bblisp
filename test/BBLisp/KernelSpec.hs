{-# LANGUAGE OverloadedStrings #-}

module BBLisp.KernelSpec
    (
      -- * Spec
      spec
    ) where

import qualified Data.ByteString.Char8 as Bsc
import Data.Map.Strict ((!))

import qualified BBLisp.Kernel as K
import BBLisp.SyntaxTree (List(..))
import Test.Hspec

-- | Spec for `Kernel`.
spec :: Spec
spec = do
    describe "str" $ do
        it "returns the string representation of true" $
            K.str [Boolean True] `shouldBe` Right (String "true")
        it "returns the string representation of false" $
            K.str [Boolean False] `shouldBe` Right (String "false")
        it "returns the string representation of Integer" $
            K.str [Integer 42] `shouldBe` Right (String "42")
        it "returns the string representation of Decimal" $
            K.str [Decimal $ read piStr]
            `shouldBe` Right (String $ Bsc.pack piStr)
        it "returns the content of String" $
            K.str [String "yolo"] `shouldBe` Right (String "yolo")
        it "returns the name of Symbol" $
            K.str [Symbol "eval"] `shouldBe` Right (String "eval")
        it "returns empty string for nil" $
            K.str [Nil] `shouldBe` Right (String "")
        it "returns the string representation of syntactic form" $
            K.str [K.bindings ! "eval"] `shouldBe` Right (String "eval")
        it "returns the string representation of function" $
            K.str [K.bindings ! "str"] `shouldBe` Right (String "str")
        it "returns the concatenation of the string representations" $
            K.str strTestList `shouldBe` Right (String "1world.")
        it "returns empty string for zero arguments" $
            K.str [] `shouldBe` Right (String "")
    describe "if'" $ do
        it "evaluates and returns then when test is evaluated to true" $
            snd <$> ifTest (Boolean True) `shouldBe` Right (String "42")
        it "evaluates and returns else when test is evaluated to false" $
            snd <$> ifTest (Boolean False) `shouldBe` Right (String "falsy")
        it "evaluates and returns nil when test is evaluated to false and there are no else" $
            snd <$> K.if' K.bindings [Boolean False, Integer 0]
            `shouldBe` Right Nil
        it "returns error for incorrect data type" $
            K.if' K.bindings [Integer 0, String "yes"]
            `shouldBe` Left "Incorrect type for `test`."
        it "returns error for no arguments" $
            K.if' K.bindings []
            `shouldBe` Left "Too few arguments to if"
        it "returns error for too few arguments" $
            K.if' K.bindings [Boolean True]
            `shouldBe` Left "Too few arguments to if"
        it "returns error for too many arguments" $
            K.if' K.bindings [Boolean True, Integer 1, Integer 2, Integer 3]
            `shouldBe` Left "Too many arguments to if"
  where
    piStr       = "3.1415926535"
    strTestList = [Integer 1, Symbol "world", String "."]
    ifTest test = K.if' K.bindings
        [ test
        , List [Symbol "str", Integer 42]
        , List [Symbol "str", String "falsy"]
        ]
