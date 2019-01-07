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
    , List
        -- First line. Test for section block with tags.
        [ List
            [ Symbol "if"
            , Symbol "$name"
            , List
                [ Symbol "str"
                , List
                    [ List
                        [ Symbol "str"
                        , String "Hello "
                        ]
                    , List
                        [ Symbol "str"
                        , Symbol "$name"
                        ]
                    , List
                        [ Symbol "str"
                        , String "."
                        ]
                    ]
                ]
            ]
        -- Second line. Test for simple section block.
        , List
            [ Symbol "str"
            , String "\n"
            ]
        , List
            [ Symbol "unless"
            , Symbol "$name"
            , List
                [ Symbol "str"
                , String "Hello world."
                ]
            ]
        -- Third line. Test for nested section blocks.
        , List
            [ Symbol "str"
            , String "\n"
            ]
        , List
            [ Symbol "with"
            , Symbol "$params"
            , List
                [ Symbol "str"
                , List
                    [ List
                        [ Symbol "if"
                        , Symbol "$$n"
                        , List
                            [ Symbol "str"
                            , Symbol "$$n"
                            ]
                        ]
                    , List
                        [ Symbol "unless"
                        , Symbol "$$n"
                        , List
                            [ Symbol "str"
                            , String "1"
                            ]
                        ]
                    ]
                ]
            ]
        -- Fourth and fifth line.
        -- Test for comment, code block, parens and integer.
        , List
            [ Symbol "str"
            , String "\n\nThe answer is "
            ]
        , List
            [ Symbol "str"
            , List
                [ Symbol "+"
                , List
                    [ Symbol "-"
                    , Integer 50
                    , Integer 20
                    ]
                , Integer 12
                ]
            ]
        -- Sixth line. Test for code block, string and decimal.
        , List
            [ Symbol "str"
            , String ".\nFirst 10 digits of "
            ]
        , List
            [ Symbol "str"
            , String "pi"
            ]
        , List
            [ Symbol "str"
            , String " is "
            ]
        , List
            [ Symbol "str"
            , Decimal $ read "3.1415926535"
            ]
        , List
            [ Symbol "str"
            , String "."
            ]
        ]
    ]
