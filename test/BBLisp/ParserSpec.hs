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
    [ Symbol "block"
    , List
        -- First line. Test for section block with tags.
        [ List
            [ Symbol "if"
            , Symbol "$name"
            , List
                [ Symbol "block"
                , List
                    [ List
                        [ Symbol "print"
                        , String "Hello "
                        ]
                    , List
                        [ Symbol "print"
                        , Symbol "$name"
                        ]
                    , List
                        [ Symbol "print"
                        , String "."
                        ]
                    ]
                ]
            ]
        -- Second line. Test for simple section block.
        , List
            [ Symbol "print"
            , String "\n"
            ]
        , List
            [ Symbol "unless"
            , Symbol "$name"
            , List
                [ Symbol "print"
                , String "Hello world."
                ]
            ]
        -- Third line. Test for nested section blocks.
        , List
            [ Symbol "print"
            , String "\n"
            ]
        , List
            [ Symbol "with"
            , Symbol "$params"
            , List
                [ Symbol "block"
                , List
                    [ List
                        [ Symbol "if"
                        , Symbol "$$n"
                        , List
                            [ Symbol "print"
                            , Symbol "$$n"
                            ]
                        ]
                    , List
                        [ Symbol "unless"
                        , Symbol "$$n"
                        , List
                            [ Symbol "print"
                            , String "1"
                            ]
                        ]
                    ]
                ]
            ]
        -- Fourth and fifth line.
        -- Test for comment, code block, parens and integer.
        , List
            [ Symbol "print"
            , String "\n\nThe answer is "
            ]
        , List
            [ Symbol "print"
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
            [ Symbol "print"
            , String ".\nFirst 10 digits of "
            ]
        , List
            [ Symbol "print"
            , String "pi"
            ]
        , List
            [ Symbol "print"
            , String " is "
            ]
        , List
            [ Symbol "print"
            , Decimal $ read "3.1415926535"
            ]
        , List
            [ Symbol "print"
            , String "."
            ]
        ]
    ]
