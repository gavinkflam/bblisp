module BBLisp.LexerSpec
    (
      -- * Spec
      spec
    ) where

import Data.List (intercalate)

import BBLisp.Lexer (Lexeme(..), LexemeClass(..), runLexer)
import Test.Hspec

-- | Spec for `Lexer`.
spec :: Spec
spec =
    describe "showFixed" $
        it "tokenizes sample template containing simple primitive tokens" $
            tokens (runLexer sampleTemplate1) `shouldBe` expectedTokens1

-- | Extract tokens from lexemes.
tokens :: Either String [Lexeme] -> [LexemeClass]
tokens (Left _)   = []
tokens (Right ls) = map f ls
  where
    f (Lexeme _ t _) = t

-- | Sample template containing simple primitive tokens.
sampleTemplate1 :: String
sampleTemplate1 = intercalate "\n"
    [ "Hello {{ name }}."
    , "{{! I am invisible }}"
    , "The answer is {{ + (- 50 20) 12 }}."
    , "First 10 digits of {{ $pi }} is {{ 3.1415926535 }}."
    ]

-- | Expected tokens scanned from `sampleTemplate1`.
expectedTokens1 :: [LexemeClass]
expectedTokens1 =
    [ LText "Hello "
    , LIdentifier "name"
    , LText ".\n\nThe answer is "
    , LIdentifier "+"
    , LLParen
    , LIdentifier "-"
    , LInteger 50
    , LInteger 20
    , LRParen
    , LInteger 12
    , LText ".\nFirst 10 digits of "
    , LIdentifier "$pi"
    , LText " is "
    , LDecimal $ read "3.1415926535"
    , LText "."
    , LEOF
    ]
