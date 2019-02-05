{-# LANGUAGE OverloadedStrings #-}

module BBLisp.KernelSpec
    (
      -- * Spec
      spec
    ) where

import qualified Data.ByteString.Char8 as Bsc
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector

import qualified BBLisp.Kernel as K
import BBLisp.SyntaxTree (List(..), Primitive(..))
import Test.Hspec

-- | Spec for `Kernel`.
spec :: Spec
spec = do
    describe "eval" $ do
        it "eval nil as idempotent" $
            evalValTest [Nil] `shouldBe` Right Nil
        it "eval boolean as idempotent" $
            evalValTest [Boolean True] `shouldBe` Right (Boolean True)
        it "eval integer as idempotent" $
            evalValTest [Integer 1] `shouldBe` Right (Integer 1)
        it "eval decimal as idempotent" $
            evalValTest [piDec] `shouldBe` Right piDec
        it "eval string as idempotent" $
            evalValTest [String "yolo"] `shouldBe` Right (String "yolo")
        it "eval dictionary as idempotent" $
            evalValTest [testDict] `shouldBe` Right testDict
        it "eval vector as idempotent" $
            evalValTest [testVector] `shouldBe` Right testVector
        it "eval symbol to resolve binding" $
            evalValTest [Symbol "eval"]
            `shouldBe` Right (Primitive $ Syntax "eval" K.eval)
        it "returns error for symbol without binding" $
            evalValTest [Symbol "exec"]
            `shouldBe` Left "Binding for 'exec' not found"
        it "find the value at a key for the dictionary" $
            evalValTest [List [testDict, String "foo"]]
            `shouldBe` Right (Integer 42)
        it "returns nil for element not in dictionary" $
            evalValTest [List [testDict, String "bar"]]
            `shouldBe` Right Nil
        it "returns error if key is not a string" $
            evalValTest [List [testDict, Integer 1]]
            `shouldBe` Left "Incorrect type for key"
        it "find the value at an index for the vector" $
            evalValTest [List [testVector, Integer 4]]
            `shouldBe` Right (Integer 5)
        it "returns nil for element not in vector" $
            evalValTest [List [testVector, Integer 10]] `shouldBe` Right Nil
        it "returns error if index is not an integer" $
            evalValTest [List [testVector, String "4"]]
            `shouldBe` Left "Incorrect type for index"
        it "applies binding and return the result" $
            evalValTest [List [Symbol "eval", Integer 1010]]
            `shouldBe` Right (Integer 1010)
        it "applies function and return the result" $
            evalValTest [List [Symbol "str", Integer 1010, String "++"]]
            `shouldBe` Right (String "1010++")
        it "returns error for unexpected form" $
            evalValTest [Integer 0, Integer 1]
            `shouldBe` Left "Unexpected form (eval 0 1)"
        it "returns error for unexpected form" $
            evalValTest [List [Integer 0, Integer 1]]
            `shouldBe` Left "Unexpected form (eval (0 1))"
    describe "str" $ do
        it "returns the string representation of true" $
            K.str [Boolean True] `shouldBe` Right (String "true")
        it "returns the string representation of false" $
            K.str [Boolean False] `shouldBe` Right (String "false")
        it "returns the string representation of Integer" $
            K.str [Integer 42] `shouldBe` Right (String "42")
        it "returns the string representation of Decimal" $
            K.str [piDec] `shouldBe` Right (String piStr)
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
            K.if' K.bindings [] `shouldBe` Left "Too few arguments to if"
        it "returns error for too few arguments" $
            K.if' K.bindings [Boolean True]
            `shouldBe` Left "Too few arguments to if"
        it "returns error for too many arguments" $
            K.if' K.bindings [Boolean True, Integer 1, Integer 2, Integer 3]
            `shouldBe` Left "Too many arguments to if"
  where
    evalValTest t = snd <$> K.eval K.bindings t
    piStr         = "3.1415926535"
    piDec         = Decimal $ read $ Bsc.unpack piStr
    testDict      = Dict $ Map.fromList
        [("foo", Integer 42), ("lol", Integer 101)]
    testVector    = Vector $ Vector.fromList
        [Integer 1, Integer 1, Integer 2, Integer 3, Integer 5]
    strTestList   = [Integer 1, Symbol "world", String "."]
    ifTest test   = K.if' K.bindings
        [ test
        , List [Symbol "str", Integer 42]
        , List [Symbol "str", String "falsy"]
        ]
