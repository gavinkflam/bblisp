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
    [ Symbol "render-list"
    , List
        -- First line. Test for section block with tags.
        [ List
            [ Symbol "if"
            , Symbol "$name"
            , List
                [ Symbol "render-list"
                , List
                    [ List
                        [ Symbol "render"
                        , String "Hello "
                        ]
                    , List
                        [ Symbol "render"
                        , Symbol "$name"
                        ]
                    , List
                        [ Symbol "render"
                        , String "."
                        ]
                    ]
                ]
            ]
        -- Second line. Test for simple section block.
        , List
            [ Symbol "render"
            , String "\n"
            ]
        , List
            [ Symbol "unless"
            , Symbol "$name"
            , List
                [ Symbol "render"
                , String "Hello world."
                ]
            ]
        -- Third line. Test for nested section blocks.
        , List
            [ Symbol "render"
            , String "\n"
            ]
        , List
            [ Symbol "with"
            , Symbol "$params"
            , List
                [ Symbol "render-list"
                , List
                    [ List
                        [ Symbol "if"
                        , Symbol "$$n"
                        , List
                            [ Symbol "render"
                            , Symbol "$$n"
                            ]
                        ]
                    , List
                        [ Symbol "unless"
                        , Symbol "$$n"
                        , List
                            [ Symbol "render"
                            , String "1"
                            ]
                        ]
                    ]
                ]
            ]
        -- Fourth and fifth line.
        -- Test for comment, code block, parens and integer.
        , List
            [ Symbol "render"
            , String "\n\nThe answer is "
            ]
        , List
            [ Symbol "render"
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
            [ Symbol "render"
            , String ".\nFirst 10 digits of "
            ]
        , List
            [ Symbol "render"
            , String "pi"
            ]
        , List
            [ Symbol "render"
            , String " is "
            ]
        , List
            [ Symbol "render"
            , Decimal $ read "3.1415926535"
            ]
        , List
            [ Symbol "render"
            , String "."
            ]
        ]
    ]
