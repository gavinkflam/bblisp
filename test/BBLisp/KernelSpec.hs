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
import BBLisp.SyntaxTree (BList(..), BPrimitive(..))
import Test.Hspec

-- | Spec for `Kernel`.
spec :: Spec
spec = do
    describe "eval" $ do
        it "eval nil as idempotent" $
            evalValTest [BNil] `shouldBe` Right BNil
        it "eval boolean as idempotent" $
            evalValTest [BBoolean True] `shouldBe` Right (BBoolean True)
        it "eval integer as idempotent" $
            evalValTest [BInteger 1] `shouldBe` Right (BInteger 1)
        it "eval decimal as idempotent" $
            evalValTest [piDec] `shouldBe` Right piDec
        it "eval string as idempotent" $
            evalValTest [BString "yolo"] `shouldBe` Right (BString "yolo")
        it "eval dictionary as idempotent" $
            evalValTest [testDict] `shouldBe` Right testDict
        it "eval vector as idempotent" $
            evalValTest [testVector] `shouldBe` Right testVector
        it "eval symbol to resolve binding" $
            evalValTest [BSymbol "eval"]
            `shouldBe` Right (BPrimitive $ BSyntax "eval" K.eval)
        it "returns error for symbol without binding" $
            evalValTest [BSymbol "exec"]
            `shouldBe` Left "Binding for 'exec' not found"
        it "find the value at a key for the dictionary" $
            evalValTest [BList [testDict, BString "foo"]]
            `shouldBe` Right (BInteger 42)
        it "returns nil for element not in dictionary" $
            evalValTest [BList [testDict, BString "bar"]]
            `shouldBe` Right BNil
        it "returns error if key is not a string" $
            evalValTest [BList [testDict, BInteger 1]]
            `shouldBe` Left "Incorrect type for key"
        it "find the value at an index for the vector" $
            evalValTest [BList [testVector, BInteger 4]]
            `shouldBe` Right (BInteger 5)
        it "returns nil for element not in vector" $
            evalValTest [BList [testVector, BInteger 10]] `shouldBe` Right BNil
        it "returns error if index is not an integer" $
            evalValTest [BList [testVector, BString "4"]]
            `shouldBe` Left "Incorrect type for index"
        it "applies binding and return the result" $
            evalValTest [BList [BSymbol "eval", BInteger 1010]]
            `shouldBe` Right (BInteger 1010)
        it "applies function and return the result" $
            evalValTest [BList [BSymbol "str", BInteger 1010, BString "++"]]
            `shouldBe` Right (BString "1010++")
        it "returns error for unexpected form" $
            evalValTest [BInteger 0, BInteger 1]
            `shouldBe` Left "Unexpected form (eval 0 1)"
        it "returns error for unexpected form" $
            evalValTest [BList [BInteger 0, BInteger 1]]
            `shouldBe` Left "Unexpected form (eval (0 1))"
    describe "str" $ do
        it "returns the string representation of true" $
            K.str [BBoolean True] `shouldBe` Right (BString "true")
        it "returns the string representation of false" $
            K.str [BBoolean False] `shouldBe` Right (BString "false")
        it "returns the string representation of Integer" $
            K.str [BInteger 42] `shouldBe` Right (BString "42")
        it "returns the string representation of Decimal" $
            K.str [piDec] `shouldBe` Right (BString piStr)
        it "returns the content of String" $
            K.str [BString "yolo"] `shouldBe` Right (BString "yolo")
        it "returns the name of Symbol" $
            K.str [BSymbol "eval"] `shouldBe` Right (BString "eval")
        it "returns empty string for nil" $
            K.str [BNil] `shouldBe` Right (BString "")
        it "returns the string representation of syntactic form" $
            K.str [K.bindings ! "eval"] `shouldBe` Right (BString "eval")
        it "returns the string representation of function" $
            K.str [K.bindings ! "str"] `shouldBe` Right (BString "str")
        it "returns the concatenation of the string representations" $
            K.str strTestList `shouldBe` Right (BString "1world.")
        it "returns empty string for zero arguments" $
            K.str [] `shouldBe` Right (BString "")
    describe "if'" $ do
        it "evaluates and returns then when test is evaluated to true" $
            snd <$> ifTest (BBoolean True) `shouldBe` Right (BString "42")
        it "evaluates and returns else when test is evaluated to false" $
            snd <$> ifTest (BBoolean False) `shouldBe` Right (BString "falsy")
        it "evaluates and returns nil when test is evaluated to false and there are no else" $
            snd <$> K.if' K.bindings [BBoolean False, BInteger 0]
            `shouldBe` Right BNil
        it "returns error for incorrect data type" $
            K.if' K.bindings [BInteger 0, BString "yes"]
            `shouldBe` Left "Incorrect type for `test`."
        it "returns error for no arguments" $
            K.if' K.bindings [] `shouldBe` Left "Too few arguments to if"
        it "returns error for too few arguments" $
            K.if' K.bindings [BBoolean True]
            `shouldBe` Left "Too few arguments to if"
        it "returns error for too many arguments" $
            K.if' K.bindings [BBoolean True, BInteger 1, BInteger 2, BInteger 3]
            `shouldBe` Left "Too many arguments to if"
    describe "get" $ do
        it "returns the value mapped to the key" $
            K.get [testDict, BString "foo"] `shouldBe` Right (BInteger 42)
        it "returns nil for key not present" $
            K.get [testDict, BString "bar"] `shouldBe` Right BNil
        it "returns error for too few arguments" $
            K.get [testDict] `shouldBe` Left "Too few arguments to get"
        it "returns nil for incorrect data type for dictionary" $
            K.get [BNil, BString "foo"] `shouldBe` Right BNil
        it "returns nil for incorrect data type for key" $
            K.get [testDict, BSymbol "foo"] `shouldBe` Right BNil
        it "returns error for too many arguments" $
            K.get [testDict, BString "bar", BString "lol"]
            `shouldBe` Left "Too many arguments to get"
  where
    evalValTest t = snd <$> K.eval K.bindings t
    piStr         = "3.1415926535"
    piDec         = BDecimal $ read $ Bsc.unpack piStr
    testDict      = BDict $ Map.fromList
        [("foo", BInteger 42), ("lol", BInteger 101)]
    testVector    = BVector $ Vector.fromList
        [BInteger 1, BInteger 1, BInteger 2, BInteger 3, BInteger 5]
    strTestList   = [BInteger 1, BSymbol "world", BString "."]
    ifTest test   = K.if' K.bindings
        [ test
        , BList [BSymbol "str", BInteger 42]
        , BList [BSymbol "str", BString "falsy"]
        ]
