module BBLisp.ParserSpec
    (
      -- * Spec
      spec
    ) where

import BBLisp.Parser (runParser)
import BBLisp.SyntaxTree (Datum(..), TemplateClass(..))
import Test.Hspec

import Templates (sampleTemplate1)

-- | Spec for `Parser`.
spec :: Spec
spec =
    describe "runParser" $
        it "parses sample template containing all syntax types" $
            astOf (runParser sampleTemplate1) `shouldBe` sample1STree

-- | Extract syntax tree from result.
astOf :: Either String TemplateClass -> TemplateClass
astOf (Left msg) = error $ "Parser error: " ++ msg
astOf (Right t)  = t

-- | Expected syntax tree for `sampleTemplate1`.
sample1STree :: TemplateClass
sample1STree = Template
    -- First line. Test for section block.
    [ Section sName $ Template
        [ Text "Hello "
        , Print sName
        , Text "."
        ]
    -- Second line. Test for invert section block.
    , Text "\n"
    , InvertSection sName $ Text "Hello world."
    -- Third line. Test for nested section blocks.
    , Text "\n"
    , Section (Symbol "$params") $ Template
        [ Section sN $ Print sN
        , InvertSection sN $ Text "1"
        ]
    -- Fourth and fifth line. Test for comment, code block, parens and integer.
    , Text "\n\nThe answer is "
    , Print $ List
        [ Symbol "+"
        , List
            [ Symbol "-"
            , Integer 50
            , Integer 20
            ]
        , Integer 12
        ]
    -- Sixth line. Test for code block, string and decimal.
    , Text ".\nFirst 10 digits of "
    , Print $ String "pi"
    , Text " is "
    , Print $ Decimal $ read "3.1415926535"
    , Text "."
    ]
  where
    sName   = Symbol "$name"
    sN      = Symbol "$$n"
