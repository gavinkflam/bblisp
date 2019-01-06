module BBLisp.LexerSpec
    (
      -- * Spec
      spec
    ) where

import Data.List (intercalate)

import BBLisp.LexemeClass (LexemeClass(..))
import BBLisp.Lexer (Lexeme(..), alexMonadScan', runAlex)
import Test.Hspec

import Templates (sampleTemplate1)

-- | Spec for `Lexer`.
spec :: Spec
spec =
    describe "runLexer" $ do
        it "tokenizes sample template containing all tokens" $
            tokensOf (runLexer sampleTemplate1) `shouldBe` sample1Tokens
        it "returns error for unknown escape sequence in string literal" $
            runLexer invalidStringTest1 `shouldBe` Left invalidStringTest1Err
        it "returns error for invalid multiline string literal" $
            runLexer invalidStringTest2 `shouldBe` Left invalidStringTest2Err
        it "returns error for unclosed code block" $
            runLexer unclosedTest1 `shouldBe` Left unclosedTest1Err
        it "returns error for unclosed comment block" $
            runLexer unclosedTest2 `shouldBe` Left unclosedTest2Err
        it "returns error for unclosed section block" $
            runLexer unclosedTest3 `shouldBe` Left unclosedTest3Err
        it "returns error for unclosed invert section block" $
            runLexer unclosedTest4 `shouldBe` Left unclosedTest4Err
        it "returns error for unclosed string literal" $
            runLexer unclosedTest5 `shouldBe` Left unclosedTest5Err
        it "returns error for unmatched closing section tag" $
            runLexer unmatchedTest1 `shouldBe` Left unmatchedTest1Err
        it "returns error for unmatched closing invert section tag" $
            runLexer unmatchedTest2 `shouldBe` Left unmatchedTest2Err
        it "returns error for unmatched nested closing section tag" $
            runLexer unmatchedTest3 `shouldBe` Left unmatchedTest3Err
        it "returns error in the middle of line" $
            runLexer locationTest1 `shouldBe` Left locationTest1Err
        it "returns error before end of line" $
            runLexer locationTest2 `shouldBe` Left locationTest2Err
        it "returns error before end of file" $
            runLexer locationTest3 `shouldBe` Left locationTest3Err

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

-- | Expected tokens for `sampleTemplate1`.
sample1Tokens :: [LexemeClass]
sample1Tokens =
    -- First line. Test for section block.
    [ LLMustachePound
    , LIdentifier "if"
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
    , LLMustachePound
    , LIdentifier "unless"
    , LIdentifier "$name"
    , LRMustache
    , LText "Hello world."
    , LCloseMustachePound
    -- Third line. Test for nested section blocks.
    , LText "\n"
    , LLMustachePound
    , LIdentifier "with"
    , LIdentifier "$params"
    , LRMustache
    , LLMustachePound
    , LIdentifier "if"
    , LIdentifier "$$n"
    , LRMustache
    , LLMustache
    , LIdentifier "$$n"
    , LRMustache
    , LCloseMustachePound
    , LLMustachePound
    , LIdentifier "unless"
    , LIdentifier "$$n"
    , LRMustache
    , LText "1"
    , LCloseMustachePound
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
invalidStringTest1Err :: String
invalidStringTest1Err =
    "Unknown escape sequence '\\i' at 1:18 on character 'i' before `\" }}`"

-- | Sample template with invalid multiline string literal.
invalidStringTest2 :: String
invalidStringTest2 = "Hello {{ \"worl\nd\" }}"

-- | Expected error for `invalidStringTest2`.
invalidStringTest2Err :: String
invalidStringTest2Err =
    "Invalid multiline string literal at 2:1 on character '\\n' before `d\" }}`"

-- | Sample template with unclosed code block.
unclosedTest1 :: String
unclosedTest1 = "Hello {{ name"

-- | Expected error for `unclosedTest1`.
unclosedTest1Err :: String
unclosedTest1Err = "Unclosed code block at end of file"

-- | Sample template with unclosed comment block.
unclosedTest2 :: String
unclosedTest2 = "Hello {{! insert name here"

-- | Expected error for `unclosedTest2`.
unclosedTest2Err :: String
unclosedTest2Err = "Unclosed comment block at end of file"

-- | Sample template with unclosed section block.
unclosedTest3 :: String
unclosedTest3 = "Hello {{# name }}{{ name }}"

-- | Expected error for `unclosedTest3`.
unclosedTest3Err :: String
unclosedTest3Err = "Unclosed section block at end of file"

-- | Sample template with unclosed invert section block.
unclosedTest4 :: String
unclosedTest4 = "Hello {{^ name }}world"

-- | Expected error for `unclosedTest4`.
unclosedTest4Err :: String
unclosedTest4Err = "Unclosed invert section block at end of file"

-- | Sample template with unclosed invert section block.
unclosedTest5 :: String
unclosedTest5 = "Hello {{ \"world"

-- | Expected error for `unclosedTest5`.
unclosedTest5Err :: String
unclosedTest5Err = "Unclosed string literal at end of file"

-- | Sample template with unmatched closing section block.
unmatchedTest1 :: String
unmatchedTest1 = "Hello {{# name }}{{ name }}{{/^}}"

-- | Expected error for `unmatchedTest1`.
unmatchedTest1Err :: String
unmatchedTest1Err = "Unmatched closing tag {{/^}} at 1:34 before end of file"

-- | Sample template with unmatched closing invert section block.
unmatchedTest2 :: String
unmatchedTest2 = "Hello {{^ name }}world{{/#}}"

-- | Expected error for `unmatchedTest2`.
unmatchedTest2Err :: String
unmatchedTest2Err = "Unmatched closing tag {{/#}} at 1:29 before end of file"

-- | Sample template with unmatched nested closing section block.
unmatchedTest3 :: String
unmatchedTest3 = "Hello {{# name }}{{^ name }}world{{/#}}{{/^}}"

-- | Expected error for `unmatchedTest3`.
unmatchedTest3Err :: String
unmatchedTest3Err =
    "Unmatched closing tag {{/#}} at 1:40 on character '}' before `{{/^}}`"

-- | Sample template with error in the middle of line.
locationTest1 :: String
locationTest1 = "Hello {{^ name }}world{{/#}}. Goodbye."

-- | Expected error for `locationTest1`.
locationTest1Err :: String
locationTest1Err =
    "Unmatched closing tag {{/#}} at 1:29 on character '}' before `. Goodbye.`"

-- | Sample template with error before end of line.
locationTest2 :: String
locationTest2 = intercalate "\n"
    [ "Hello {{^ name }}world{{/#}}"
    , "and goodbye."
    ]

-- | Expected error for `locationTest2`.
locationTest2Err :: String
locationTest2Err = "Unmatched closing tag {{/#}} at 1:29 before end of line"

-- | Sample template with error before end of file.
locationTest3 :: String
locationTest3 = "Hello world{{/^}}"

-- | Expected error for `locationTest3`.
locationTest3Err :: String
locationTest3Err = "Unmatched closing tag {{/^}} at 1:18 before end of file"
