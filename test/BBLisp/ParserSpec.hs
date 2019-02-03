{-# LANGUAGE OverloadedStrings #-}

module BBLisp.ParserSpec
    (
      -- * Spec
      spec
    ) where

import qualified Data.Map.Strict as Map

import BBLisp.Parser (runParser)
import BBLisp.SyntaxTree (List(..))
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
astOf :: Either String List -> List
astOf (Left msg) = error $ "Parser error: " ++ msg
astOf (Right t)  = t

-- | Expected syntax tree for `tempBool`.
astBool :: List
astBool = List
    [ Symbol "str"
    , String "This is "
    , Boolean True
    , String ". That is "
    , Boolean False
    , String "."
    ]

-- | Expected syntax tree for `tempNil`.
astNil :: List
astNil = List
    [ Symbol "str"
    , String "Nothing is here, except "
    , Nil
    , String "."
    ]

-- | Expected syntax tree for `tempLit`.
astLit :: List
astLit = List
    [ Symbol "str"
    , String "First "
    , Integer 10
    , String " digits of "
    , String "pi"
    , String " is "
    , Decimal $ read "3.1415926535"
    , String "."
    ]

-- | Expected syntax tree for `tempDict1`.
astDict1 :: List
astDict1 = List
    [ Symbol "str"
    , String "There are "
    , List
        [ Dict $ Map.fromList
            [ ("apples", Integer 5)
            , ("oranges", Integer 2)
            ]
        , String "apples"
        ]
    , String " apples."
    ]

-- | Expected syntax tree for `tempDict2`.
astDict2 :: List
astDict2 = List
    [ Symbol "str"
    , String "Earth weights "
    , List
        [ Symbol "get-in"
        , Symbol "$planets"
        , List
            [ String "earth"
            , String "weight"
            ]
        ]
    , String " kg."
    ]

-- | Expected syntax tree for `tempEmpty`.
astEmpty :: List
astEmpty = List [Symbol "str"]

-- | Expected syntax tree for `tempComment1`.
astComment1 :: List
astComment1 = List [Symbol "str"]

-- | Expected syntax tree for `tempComment2`.
astComment2 :: List
astComment2 = List
    [ Symbol "str"
    , String "hello world."
    ]

-- | Expected syntax tree for `tempIf`.
astIf :: List
astIf = List
    [ Symbol "str"
    , List
        [ Symbol "if"
        , List
            [ Symbol "defined?"
            , Symbol "$name"
            ]
        , List
            [ Symbol "str"
            , String "Hello "
            , Symbol "$name"
            , String "."
            ]
        ]
    ]

-- | Expected syntax tree for `tempUnless`.
astUnless :: List
astUnless = List
    [ Symbol "str"
    , List
        [ Symbol "unless"
        , List
            [ Symbol "defined?"
            , Symbol "$name"
            ]
        , String "Hello world."
        ]
    ]

-- | Expected syntax tree for `tempWith`.
astWith :: List
astWith = List
    [ Symbol "str"
    , List
        [ Symbol "with"
        , Symbol "$list"
        , List
            [ Symbol "str"
            , Symbol "$$element"
            , String " and "
            ]
        ]
    ]

-- | Expected syntax tree for `tempArith`.
astArith :: List
astArith = List
    [ Symbol "str"
    , String "The answer is "
    , List
        [ Symbol "+"
        , List
            [ Symbol "-"
            , Integer 50
            , Integer 20
            ]
        , Integer 12
        ]
    , String "."
    ]
