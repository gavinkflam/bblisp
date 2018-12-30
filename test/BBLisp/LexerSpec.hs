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
tokens (Left msg) = error $ "Lexer error: " ++ msg
tokens (Right ls) = map f ls
  where
    f (Lexeme _ t _) = t

-- | Sample template containing simple primitive tokens.
sampleTemplate1 :: String
sampleTemplate1 = intercalate "\n"
    [ "{{# name }}Hello {{ name }}.{{/#}}"
    , "{{^ name }}Hello world.{{/^}}"
    , "{{! I am invisible }}"
    , "The answer is {{ + (- 50 20) 12 }}."
    , "First 10 digits of {{ $pi }} is {{ 3.1415926535 }}."
    ]

-- | Expected tokens scanned from `sampleTemplate1`.
expectedTokens1 :: [LexemeClass]
expectedTokens1 =
    -- First line.
    [ LLMustachePound
    , LIdentifier "name"
    , LRMustache
    , LText "Hello "
    , LLMustache
    , LIdentifier "name"
    , LRMustache
    , LText "."
    , LCloseMustachePound
    -- Second line.
    , LText "\n"
    , LLMustacheCaret
    , LIdentifier "name"
    , LRMustache
    , LText "Hello world."
    , LCloseMustacheCaret
    -- Third and fourth line.
    , LText "\n\nThe answer is "
    , LLMustache
    , LIdentifier "+"
    , LLParen
    , LIdentifier "-"
    , LInteger 50
    , LInteger 20
    , LRParen
    , LInteger 12
    , LRMustache
    -- Fifth line.
    , LText ".\nFirst 10 digits of "
    , LLMustache
    , LIdentifier "$pi"
    , LRMustache
    , LText " is "
    , LLMustache
    , LDecimal $ read "3.1415926535"
    , LRMustache
    , LText "."
    , LEOF
    ]
