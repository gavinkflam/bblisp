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
    describe "=" $ do
        it "returns true for nils" $
            K.eq [BNil, BNil] `shouldBe` Right (BBoolean True)
        it "returns true for equivalent booleans" $
            K.eq [BBoolean False, BBoolean False]
            `shouldBe` Right (BBoolean True)
        it "returns false for nonequivalent booleans" $
            K.eq [BBoolean True, BBoolean False]
            `shouldBe` Right (BBoolean False)
        it "returns true for equivalent integers" $
            K.eq [BInteger 42, BInteger 42] `shouldBe` Right (BBoolean True)
        it "returns false for nonequivalent integers" $
            K.eq [BInteger 30, BInteger 42] `shouldBe` Right (BBoolean False)
        it "returns true for equivalent decimals" $
            K.eq [BDecimal $ read "3.14", BDecimal $ read "3.14"]
            `shouldBe` Right (BBoolean True)
        it "returns false for nonequivalent decimals" $
            K.eq [BDecimal $ read "3.14", BDecimal $ read "1.618"]
            `shouldBe` Right (BBoolean False)
        it "returns true for equivalent strings" $
            K.eq [BString "foo", BString "foo"]
            `shouldBe` Right (BBoolean True)
        it "returns false for nonequivalent strings" $
            K.eq [BString "foo", BString "bar"]
            `shouldBe` Right (BBoolean False)
        it "returns true for equivalent symbols" $
            K.eq [BSymbol "foo", BSymbol "foo"]
            `shouldBe` Right (BBoolean True)
        it "returns false for nonequivalent symbols" $
            K.eq [BSymbol "foo", BSymbol "bar"]
            `shouldBe` Right (BBoolean False)
        it "returns true for equivalent dictionaries" $
            K.eq [testDict, testDict] `shouldBe` Right (BBoolean True)
        it "returns false for nonequivalent dictionaries" $
            K.eq [testDict, BDict mempty] `shouldBe` Right (BBoolean False)
        it "returns true for equivalent vectors" $
            K.eq [testVector, testVector] `shouldBe` Right (BBoolean True)
        it "returns false for nonequivalent vectors" $
            K.eq [testVector, BVector mempty] `shouldBe` Right (BBoolean False)
        it "returns true for equivalent lists" $
            K.eq [BList [BInteger 1], BList [BInteger 1]]
            `shouldBe` Right (BBoolean True)
        it "returns false for nonequivalent lists" $
            K.eq [BList [BInteger 1], BList [BInteger 2]]
            `shouldBe` Right (BBoolean False)
        it "returns true for equivalent primitives" $
            K.eq [K.bindings ! "=", K.bindings ! "="]
            `shouldBe` Right (BBoolean True)
        it "returns false for nonequivalent primitives" $
            K.eq [K.bindings ! "=", K.bindings ! "eval"]
            `shouldBe` Right (BBoolean False)
        it "returns true for more than three equivalent integers" $
            K.eq [BInteger 42, BInteger 42, BInteger 42]
            `shouldBe` Right (BBoolean True)
        it "returns false for more than three nonequivalent integers" $
            K.eq [BInteger 30, BInteger 30, BInteger 42]
            `shouldBe` Right (BBoolean False)
        it "returns false for unmatched types" $
            K.eq [BInteger 42, BDecimal $ read "42"]
            `shouldBe` Right (BBoolean False)
        it "returns error for too few arguments" $
            K.eq [BInteger 42] `shouldBe` Left "Too few arguments to ="
    describe "+" $ do
        it "adds nothing, return zero" $
            K.add [] `shouldBe` Right (BInteger 0)
        it "adds an integer, return the same integer" $
            K.add [BInteger 42] `shouldBe` Right (BInteger 42)
        it "adds a decimal, return the same decimal" $
            K.add [BDecimal $ read "3.14"]
            `shouldBe` Right (BDecimal $ read "3.14")
        it "adds two integers, return the result as integer" $
            K.add [BInteger 42, BInteger 30] `shouldBe` Right (BInteger 72)
        it "adds two decimals, return the result as decimal" $
            K.add [BDecimal $ read "3.14", BDecimal $ read "1.618"]
            `shouldBe` Right (BDecimal $ read "4.758")
        it "adds a decimal with an integer, return the result as decimal" $
            K.add [BDecimal $ read "3.14", BInteger 42]
            `shouldBe` Right (BDecimal $ read "45.14")
        it "adds an integer with a decimal, return the result as decimal" $
            K.add [BInteger 42, BDecimal $ read "3.14"]
            `shouldBe` Right (BDecimal $ read "45.14")
        it "adds three integers, return the result as integer" $
            K.add [BInteger 42, BInteger 30, BInteger 58]
            `shouldBe` Right (BInteger 130)
        it "adds three decimals, return the result as decimal" $
            K.add
                [ BDecimal $ read "3.14"
                , BDecimal $ read "1.618"
                , BDecimal $ read "1.41421"
                ]
            `shouldBe` Right (BDecimal $ read "6.17221")
        it "adds an integer with two decimals, return the result as decimal" $
            K.add
                [ BInteger 42
                , BDecimal $ read "1.618"
                , BDecimal $ read "1.41421"
                ]
            `shouldBe` Right (BDecimal $ read "45.03221")
        it "reports error for non-numerical arguments" $
            K.add [BString "42"]
            `shouldBe` Left "Arguments should be integers or decimals"
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
            K.get [testDict, BString "bar", BString "lorem"]
            `shouldBe` Left "Too many arguments to get"
    describe "get-in" $ do
        it "returns the value in the first level of the dictionary" $
            K.getIn [testDict , BVector $ Vector.fromList [BString "foo"]]
            `shouldBe` Right (BInteger 42)
        it "returns the value in the second level of the dictionary" $
            K.getIn
                [ testDict
                , BVector $ Vector.fromList
                    [ BString "lorem"
                    , BString "dolor"
                    ]
                ]
                `shouldBe` Right (BString "amet")
        it "returns nil for key not present in the second level" $
            K.getIn
                [ testDict
                , BVector $ Vector.fromList
                    [ BString "lorem"
                    , BString "consectetur"
                    ]
                ]
                `shouldBe` Right BNil
        it "returns nil for key not present in the first level" $
            K.getIn
                [ testDict
                , BVector $ Vector.fromList
                    [ BString "ipsum"
                    ]
                ]
                `shouldBe` Right BNil
        it "returns nil for key not present in the first and second level" $
            K.getIn
                [ testDict
                , BVector $ Vector.fromList
                    [ BString "ipsum"
                    , BString "dolor"
                    ]
                ]
                `shouldBe` Right BNil
        it "returns nil for incorrect data type for dictionary" $
            K.getIn [BNil, BVector $ Vector.fromList [BString "foo"]]
            `shouldBe` Right BNil
        it "returns nil for incorrect data type for key" $
            K.getIn
                [ testDict
                , BVector $ Vector.fromList
                    [ BSymbol "lorem"
                    , BSymbol "consectetur"
                    ]
                ]
                `shouldBe` Right BNil
        it "returns nil for non-vector key sequence" $
            K.getIn [testDict, BNil] `shouldBe` Left "Keys should be vector"
        it "returns error for too few arguments" $
            K.getIn [testDict] `shouldBe` Left "Too few arguments to get-in"
        it "returns error for too many arguments" $
            K.getIn [testDict, BString "bar", BString "lorem"]
            `shouldBe` Left "Too many arguments to get-in"
  where
    evalValTest t = snd <$> K.eval K.bindings t
    piStr         = "3.1415926535"
    piDec         = BDecimal $ read $ Bsc.unpack piStr
    testDict      = BDict $ Map.fromList
        [ ("foo", BInteger 42)
        ,
            ( "lorem"
            , BDict $ Map.fromList
                [ ("ipsum", BInteger 50)
                , ("dolor", BString "amet")
                ]
            )
        ]
    testVector    = BVector $ Vector.fromList
        [BInteger 1, BInteger 1, BInteger 2, BInteger 3, BInteger 5]
    strTestList   = [BInteger 1, BSymbol "world", BString "."]
    ifTest test   = K.if' K.bindings
        [ test
        , BList [BSymbol "str", BInteger 42]
        , BList [BSymbol "str", BString "falsy"]
        ]
