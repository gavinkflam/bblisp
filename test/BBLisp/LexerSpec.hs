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
    describe "runLexer" $ do
        it "tokenizes sample template containing all tokens" $
            tokensOf (runLexer template1) `shouldBe` tokens1
        it "returns error for unclosed code block" $
            runLexer errorTemplate1 `shouldBe` Left error1
        it "returns error for unclosed comment block" $
            runLexer errorTemplate2 `shouldBe` Left error2
        it "returns error for unclosed section block" $
            runLexer errorTemplate3 `shouldBe` Left error3
        it "returns error for unclosed invert section block" $
            runLexer errorTemplate4 `shouldBe` Left error4
        it "returns error for unmatched closing section tag" $
            runLexer errorTemplate5 `shouldBe` Left error5
        it "returns error for unmatched closing invert section tag" $
            runLexer errorTemplate6 `shouldBe` Left error6
        it "returns error in the middle of line" $
            runLexer errorTemplate7 `shouldBe` Left error7
        it "returns error before end of line" $
            runLexer errorTemplate8 `shouldBe` Left error8
        it "returns error before end of file" $
            runLexer errorTemplate9 `shouldBe` Left error9

-- | Extract tokens from lexemes.
tokensOf :: Either String [Lexeme] -> [LexemeClass]
tokensOf (Left msg) = error $ "Lexer error: " ++ msg
tokensOf (Right ls) =
    map f ls
  where
    f (Lexeme _ l _) = l

-- | Sample template containing all tokens.
template1 :: String
template1 = intercalate "\n"
    [ "{{# $name }}Hello {{ $name }}.{{/#}}"
    , "{{^ $name }}Hello world.{{/^}}"
    , "{{# $params }}{{# $$n }}{{ $$n }}{{/#}}{{^ $$n }}1{{/^}}{{/#}}"
    , "{{! I am invisible }}"
    , "The answer is {{ + (- 50 20) 12 }}."
    , "First 10 digits of {{ \"pi\" }} is {{ 3.1415926535 }}."
    ]

-- | Expected tokens for `template1`.
tokens1 :: [LexemeClass]
tokens1 =
    -- First line. Test for section block.
    [ LLMustachePound
    , LIdentifier "$name"
    , LRMustache
    , LText "Hello "
    , LLMustache
    , LIdentifier "$name"
    , LRMustache
    , LText "."
    , LCloseMustachePound
    -- Second line. Test for invert section block.
    , LText "\n"
    , LLMustacheCaret
    , LIdentifier "$name"
    , LRMustache
    , LText "Hello world."
    , LCloseMustacheCaret
    -- Third line. Test for nested section blocks.
    , LText "\n"
    , LLMustachePound
    , LIdentifier "$params"
    , LRMustache
    , LLMustachePound
    , LIdentifier "$$n"
    , LRMustache
    , LLMustache
    , LIdentifier "$$n"
    , LRMustache
    , LCloseMustachePound
    , LLMustacheCaret
    , LIdentifier "$$n"
    , LRMustache
    , LText "1"
    , LCloseMustacheCaret
    , LCloseMustachePound
    -- Fourth and fifth line. Test for comment, code block, parens and integer.
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
    -- Sixth line. Test for code block, string and decimal.
    , LText ".\nFirst 10 digits of "
    , LLMustache
    , LString "pi"
    , LRMustache
    , LText " is "
    , LLMustache
    , LDecimal $ read "3.1415926535"
    , LRMustache
    , LText "."
    , LEOF
    ]

-- | Sample template with unclosed code block.
errorTemplate1 :: String
errorTemplate1 = "Hello {{ name"

-- | Expected error for `errorTemplate1`.
error1 :: String
error1 = "Unclosed code block at end of file"

-- | Sample template with unclosed comment block.
errorTemplate2 :: String
errorTemplate2 = "Hello {{! insert name here"

-- | Expected error for `errorTemplate2`.
error2 :: String
error2 = "Unclosed comment block at end of file"

-- | Sample template with unclosed section block.
errorTemplate3 :: String
errorTemplate3 = "Hello {{# name }}{{ name }}"

-- | Expected error for `errorTemplate3`.
error3 :: String
error3 = "Unclosed section block at end of file"

-- | Sample template with unclosed invert section block.
errorTemplate4 :: String
errorTemplate4 = "Hello {{^ name }}world"

-- | Expected error for `errorTemplate4`.
error4 :: String
error4 = "Unclosed invert section block at end of file"

-- | Sample template with unmatched closing section block.
errorTemplate5 :: String
errorTemplate5 = "Hello {{# name }}{{ name }}{{/^}}"

-- | Expected error for `errorTemplate5`.
error5 :: String
error5 = "Unmatched closing tag {{/^}} at 1:34 before end of file"

-- | Sample template with unmatched closing invert section block.
errorTemplate6 :: String
errorTemplate6 = "Hello {{^ name }}world{{/#}}"

-- | Expected error for `errorTemplate6`.
error6 :: String
error6 = "Unmatched closing tag {{/#}} at 1:29 before end of file"

-- | Sample template with error in the middle of line.
errorTemplate7 :: String
errorTemplate7 = "Hello {{^ name }}world{{/#}}. Goodbye."

-- | Expected error for `errorTemplate7`.
error7 :: String
error7 =
    "Unmatched closing tag {{/#}} at 1:29 on character '}' before `. Goodbye.`"

-- | Sample template with error before end of line.
errorTemplate8 :: String
errorTemplate8 = intercalate "\n"
    [ "Hello {{^ name }}world{{/#}}"
    , "and goodbye."
    ]

-- | Expected error for `errorTemplate8`.
error8 :: String
error8 = "Unmatched closing tag {{/#}} at 1:29 before end of line"

-- | Sample template with error before end of file.
errorTemplate9 :: String
errorTemplate9 = "Hello world{{/^}}"

-- | Expected error for `errorTemplate9`.
error9 :: String
error9 = "Unmatched closing tag {{/^}} at 1:18 before end of file"
