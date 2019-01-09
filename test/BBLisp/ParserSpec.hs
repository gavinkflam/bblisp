module BBLisp.ParserSpec
    (
      -- * Spec
      spec
    ) where

import BBLisp.Parser (runParser)
import BBLisp.SyntaxTree (List(..))
import Test.Hspec

import Templates (sampleTemplate1)

-- | Spec for `Parser`.
spec :: Spec
spec =
    describe "runParser" $
        it "parses sample template containing all syntax types" $
            astOf (runParser sampleTemplate1) `shouldBe` sample1STree

-- | Extract syntax tree from result.
astOf :: Either String List -> List
astOf (Left msg) = error $ "Parser error: " ++ msg
astOf (Right t)  = t

-- | Expected syntax tree for `sampleTemplate1`.
sample1STree :: List
sample1STree = List
    [ Symbol "str"
    -- First line. Test for section block with tags.
    , List
        [ Symbol "if"
        , Symbol "$name"
        , List
            [ Symbol "str"
            , String "Hello "
            , Symbol "$name"
            , String "."
            ]
        ]
    -- Second line. Test for simple section block.
    , String "\n"
    , List
        [ Symbol "unless"
        , Symbol "$name"
        , String "Hello world."
        ]
    -- Third line. Test for nested section blocks.
    , String "\n"
    , List
        [ Symbol "with"
        , Symbol "$params"
        , List
            [ Symbol "str"
            , List
                [ Symbol "if"
                , Symbol "$$n"
                , Symbol "$$n"
                ]
            , List
                [ Symbol "unless"
                , Symbol "$$n"
                , String "1"
                ]
            ]
        ]
    -- Fourth and fifth line.
    -- Test for comment, code block, parens and integer.
    , String "\n\nThe answer is "
    , List
        [ Symbol "+"
        , List
            [ Symbol "-"
            , Integer 50
            , Integer 20
            ]
        , Integer 12
        ]
    -- Sixth line. Test for code block, string and decimal.
    , String ".\nFirst 10 digits of "
    , String "pi"
    , String " is "
    , Decimal $ read "3.1415926535"
    , String "."
    ]
