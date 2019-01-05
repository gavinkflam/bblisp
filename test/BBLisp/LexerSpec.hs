module BBLisp.LexerSpec
    (
      -- * Spec
      spec
    ) where

import Data.List (intercalate)

import BBLisp.LexemeClass (LexemeClass(..))
import BBLisp.Lexer (Lexeme(..), alexMonadScan', runAlex)
import Test.Hspec

-- | Spec for `Lexer`.
spec :: Spec
spec =
    describe "runLexer" $ do
        it "tokenizes sample template containing all tokens" $
            tokensOf (runLexer tokensTest) `shouldBe` tokensTestTokens
        it "returns error for unknown escape sequence in string literal" $
            runLexer invalidStringTest1 `shouldBe` Left invalidStringTestErr1
        it "returns error for invalid multiline string literal" $
            runLexer invalidStringTest2 `shouldBe` Left invalidStringTestErr2
        it "returns error for unclosed code block" $
            runLexer unclosedTest1 `shouldBe` Left unclosedTestErr1
        it "returns error for unclosed comment block" $
            runLexer unclosedTest2 `shouldBe` Left unclosedTestErr2
        it "returns error for unclosed section block" $
            runLexer unclosedTest3 `shouldBe` Left unclosedTestErr3
        it "returns error for unclosed invert section block" $
            runLexer unclosedTest4 `shouldBe` Left unclosedTestErr4
        it "returns error for unclosed string literal" $
            runLexer unclosedTest5 `shouldBe` Left unclosedTestErr5
        it "returns error for unmatched closing section tag" $
            runLexer unmatchedTest1 `shouldBe` Left unmatchedTestErr1
        it "returns error for unmatched closing invert section tag" $
            runLexer unmatchedTest2 `shouldBe` Left unmatchedTestErr2
        it "returns error for unmatched nested closing section tag" $
            runLexer unmatchedTest3 `shouldBe` Left unmatchedTestErr3
        it "returns error in the middle of line" $
            runLexer locationTest1 `shouldBe` Left locationTestErr1
        it "returns error before end of line" $
            runLexer locationTest2 `shouldBe` Left locationTestErr2
        it "returns error before end of file" $
            runLexer locationTest3 `shouldBe` Left locationTestErr3

-- | Run the lexer to collect lexemes.
runLexer :: String -> Either String [Lexeme]
runLexer =
    flip runAlex $ collectWhileM notEof alexMonadScan'
  where
    notEof (Lexeme _ LEOF _) = False
    notEof _                 = True

-- | Collect the results of the monadic function `fM` until a result satisfy
--   the predicate `p`.
collectWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
collectWhileM p fM =
    reverse <$> loopM []
  where
    loopM xs = fM >>= (\y -> if p y then loopM (y:xs) else return $ y:xs)

-- | Extract tokens from lexemes.
tokensOf :: Either String [Lexeme] -> [LexemeClass]
tokensOf (Left msg) = error $ "Lexer error: " ++ msg
tokensOf (Right ls) =
    map f ls
  where
    f (Lexeme _ l _) = l

-- | Sample template containing all tokens.
tokensTest :: String
tokensTest = intercalate "\n"
    [ "{{# $name }}Hello {{ $name }}.{{/#}}"
    , "{{^ $name }}Hello world.{{/^}}"
    , "{{# $params }}{{# $$n }}{{ $$n }}{{/#}}{{^ $$n }}1{{/^}}{{/#}}"
    , "{{! I am invisible }}"
    , "The answer is {{ + (- 50 20) 12 }}."
    , "First 10 digits of {{ \"pi\" }} is {{ 3.1415926535 }}."
    ]

-- | Expected tokens for `tokensTest`.
tokensTestTokens :: [LexemeClass]
tokensTestTokens =
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

-- | Sample template with unknown escape sequence in string literal.
invalidStringTest1 :: String
invalidStringTest1 = "Hello {{ \"world\\i\" }}"

-- | Expected error for `invalidStringTest1`.
invalidStringTestErr1 :: String
invalidStringTestErr1 =
    "Unknown escape sequence '\\i' at 1:18 on character 'i' before `\" }}`"

-- | Sample template with invalid multiline string literal.
invalidStringTest2 :: String
invalidStringTest2 = "Hello {{ \"worl\nd\" }}"

-- | Expected error for `invalidStringTest2`.
invalidStringTestErr2 :: String
invalidStringTestErr2 =
    "Invalid multiline string literal at 2:1 on character '\\n' before `d\" }}`"

-- | Sample template with unclosed code block.
unclosedTest1 :: String
unclosedTest1 = "Hello {{ name"

-- | Expected error for `unclosedTest1`.
unclosedTestErr1 :: String
unclosedTestErr1 = "Unclosed code block at end of file"

-- | Sample template with unclosed comment block.
unclosedTest2 :: String
unclosedTest2 = "Hello {{! insert name here"

-- | Expected error for `unclosedTest2`.
unclosedTestErr2 :: String
unclosedTestErr2 = "Unclosed comment block at end of file"

-- | Sample template with unclosed section block.
unclosedTest3 :: String
unclosedTest3 = "Hello {{# name }}{{ name }}"

-- | Expected error for `unclosedTest3`.
unclosedTestErr3 :: String
unclosedTestErr3 = "Unclosed section block at end of file"

-- | Sample template with unclosed invert section block.
unclosedTest4 :: String
unclosedTest4 = "Hello {{^ name }}world"

-- | Expected error for `unclosedTest4`.
unclosedTestErr4 :: String
unclosedTestErr4 = "Unclosed invert section block at end of file"

-- | Sample template with unclosed invert section block.
unclosedTest5 :: String
unclosedTest5 = "Hello {{ \"world"

-- | Expected error for `unclosedTest5`.
unclosedTestErr5 :: String
unclosedTestErr5 = "Unclosed string literal at end of file"

-- | Sample template with unmatched closing section block.
unmatchedTest1 :: String
unmatchedTest1 = "Hello {{# name }}{{ name }}{{/^}}"

-- | Expected error for `unmatchedTest1`.
unmatchedTestErr1 :: String
unmatchedTestErr1 = "Unmatched closing tag {{/^}} at 1:34 before end of file"

-- | Sample template with unmatched closing invert section block.
unmatchedTest2 :: String
unmatchedTest2 = "Hello {{^ name }}world{{/#}}"

-- | Expected error for `unmatchedTest2`.
unmatchedTestErr2 :: String
unmatchedTestErr2 = "Unmatched closing tag {{/#}} at 1:29 before end of file"

-- | Sample template with unmatched nested closing section block.
unmatchedTest3 :: String
unmatchedTest3 = "Hello {{# name }}{{^ name }}world{{/#}}{{/^}}"

-- | Expected error for `unmatchedTest3`.
unmatchedTestErr3 :: String
unmatchedTestErr3 =
    "Unmatched closing tag {{/#}} at 1:40 on character '}' before `{{/^}}`"

-- | Sample template with error in the middle of line.
locationTest1 :: String
locationTest1 = "Hello {{^ name }}world{{/#}}. Goodbye."

-- | Expected error for `locationTest1`.
locationTestErr1 :: String
locationTestErr1 =
    "Unmatched closing tag {{/#}} at 1:29 on character '}' before `. Goodbye.`"

-- | Sample template with error before end of line.
locationTest2 :: String
locationTest2 = intercalate "\n"
    [ "Hello {{^ name }}world{{/#}}"
    , "and goodbye."
    ]

-- | Expected error for `locationTest2`.
locationTestErr2 :: String
locationTestErr2 = "Unmatched closing tag {{/#}} at 1:29 before end of line"

-- | Sample template with error before end of file.
locationTest3 :: String
locationTest3 = "Hello world{{/^}}"

-- | Expected error for `locationTest3`.
locationTestErr3 :: String
locationTestErr3 = "Unmatched closing tag {{/^}} at 1:18 before end of file"
