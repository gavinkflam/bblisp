{-# LANGUAGE OverloadedStrings #-}

module BBLisp.ParserSpec
    (
      -- * Spec
      spec
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector

import BBLisp.Parser (runParser)
import BBLisp.SyntaxTree (BList(..))
import Test.Hspec

import qualified Templates as Tmp

-- | Spec for `Parser`.
spec :: Spec
spec =
    describe "parses templates into syntax tree" $ do
        it "parses template containing booleans" $
            astOf (runParser Tmp.tempBool) `shouldBe` astBool
        it "parses template containing nil" $
            astOf (runParser Tmp.tempNil) `shouldBe` astNil
        it "parses template containing literals" $
            astOf (runParser Tmp.tempLit) `shouldBe` astLit
        it "parses template containing multiple strings" $
            astOf (runParser Tmp.tempStrings) `shouldBe` astStrings
        it "parses template containing vector literal" $
            astOf (runParser Tmp.tempVector1) `shouldBe` astVector1
        it "parses template containing nested vector" $
            astOf (runParser Tmp.tempVector2) `shouldBe` astVector2
        it "parses template containing dictionary literal" $
            astOf (runParser Tmp.tempDict1) `shouldBe` astDict1
        it "parses template containing nested dictionary" $
            astOf (runParser Tmp.tempDict2) `shouldBe` astDict2
        it "parses empty template" $
            astOf (runParser Tmp.tempEmpty) `shouldBe` astEmpty
        it "parses template containing only comment" $
            astOf (runParser Tmp.tempComment1) `shouldBe` astComment1
        it "parses template containing comment" $
            astOf (runParser Tmp.tempComment2) `shouldBe` astComment2
        it "parses template containing if" $
            astOf (runParser Tmp.tempIf) `shouldBe` astIf
        it "parses template containing unless" $
            astOf (runParser Tmp.tempUnless) `shouldBe` astUnless
        it "parses template containing with" $
            astOf (runParser Tmp.tempWith) `shouldBe` astWith
        it "parses template containing arithmetic" $
            astOf (runParser Tmp.tempArith) `shouldBe` astArith

-- | Extract syntax tree from result.
astOf :: Either String BList -> BList
astOf (Left msg) = error $ "Parser error: " ++ msg
astOf (Right t)  = t

-- | Expected syntax tree for `tempBool`.
astBool :: BList
astBool = BList
    [ BSymbol "str"
    , BString "This is "
    , BBoolean True
    , BString ". That is "
    , BBoolean False
    , BString "."
    ]

-- | Expected syntax tree for `tempNil`.
astNil :: BList
astNil = BList
    [ BSymbol "str"
    , BString "Nothing is here, except "
    , BNil
    , BString "."
    ]

-- | Expected syntax tree for `tempLit`.
astLit :: BList
astLit = BList
    [ BSymbol "str"
    , BString "First "
    , BInteger 10
    , BString " digits of "
    , BString "pi"
    , BString " is "
    , BDecimal $ read "3.1415926535"
    , BString "."
    ]

-- | Expected syntax tree for `tempStrings`.
astStrings :: BList
astStrings = BList
    [ BSymbol "str"
    , BString "Hello"
    , BString " "
    , BString "world"
    ]

-- | Expected syntax tree for `tempVector1`.
astVector1 :: BList
astVector1 = BList
    [ BSymbol "str"
    , BString "Third prime number is "
    , BList
        [ BVector $ Vector.fromList [BInteger 2, BInteger 3, BInteger 5]
        , BInteger 2
        ]
    , BString "."
    ]

-- | Expected syntax tree for `tempVector2`.
astVector2 :: BList
astVector2 = BList
    [ BSymbol "str"
    , BString "Grid (1, 2) is "
    , BList
        [ BSymbol "get-in"
        , BVector $ Vector.fromList
            [ BVector $ Vector.fromList [BInteger 1, BInteger 2]
            , BVector $ Vector.fromList [BInteger 3, BInteger 4]
            ]
        , BVector $ Vector.fromList [BInteger 1, BInteger 2]
        ]
    , BString "."
    ]

-- | Expected syntax tree for `tempDict1`.
astDict1 :: BList
astDict1 = BList
    [ BSymbol "str"
    , BString "There are "
    , BList
        [ BDict $ Map.fromList
            [ ("apples", BInteger 5)
            , ("oranges", BInteger 2)
            ]
        , BString "apples"
        ]
    , BString " apples."
    ]

-- | Expected syntax tree for `tempDict2`.
astDict2 :: BList
astDict2 = BList
    [ BSymbol "str"
    , BString "Earth weights "
    , BList
        [ BSymbol "get-in"
        , BSymbol "$planets"
        , BVector $ Vector.fromList [BString "earth", BString "weight"]
        ]
    , BString " kg."
    ]

-- | Expected syntax tree for `tempEmpty`.
astEmpty :: BList
astEmpty = BList [BSymbol "str"]

-- | Expected syntax tree for `tempComment1`.
astComment1 :: BList
astComment1 = BList [BSymbol "str"]

-- | Expected syntax tree for `tempComment2`.
astComment2 :: BList
astComment2 = BList
    [ BSymbol "str"
    , BString "hello world."
    ]

-- | Expected syntax tree for `tempIf`.
astIf :: BList
astIf = BList
    [ BSymbol "str"
    , BList
        [ BSymbol "if"
        , BList
            [ BSymbol "defined?"
            , BSymbol "$name"
            ]
        , BList
            [ BSymbol "str"
            , BString "Hello "
            , BSymbol "$name"
            , BString "."
            ]
        ]
    ]

-- | Expected syntax tree for `tempUnless`.
astUnless :: BList
astUnless = BList
    [ BSymbol "str"
    , BList
        [ BSymbol "unless"
        , BList
            [ BSymbol "defined?"
            , BSymbol "$name"
            ]
        , BString "Hello world."
        ]
    ]

-- | Expected syntax tree for `tempWith`.
astWith :: BList
astWith = BList
    [ BSymbol "str"
    , BList
        [ BSymbol "with"
        , BSymbol "$list"
        , BList
            [ BSymbol "str"
            , BSymbol "$$element"
            , BString " and "
            ]
        ]
    ]

-- | Expected syntax tree for `tempArith`.
astArith :: BList
astArith = BList
    [ BSymbol "str"
    , BString "The answer is "
    , BList
        [ BSymbol "+"
        , BList
            [ BSymbol "-"
            , BInteger 50
            , BInteger 20
            ]
        , BInteger 12
        ]
    , BString "."
    ]
