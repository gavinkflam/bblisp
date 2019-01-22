module BBLisp.LexerSpec
    (
      -- * Spec
      spec
    ) where

import BBLisp.LexemeClass (LexemeClass(..))
import BBLisp.Lexer (Lexeme(..), alexMonadScan', runAlex)
import Test.Hspec

import qualified Templates as Tmp

-- | Spec for `Lexer`.
spec :: Spec
spec = do
    describe "tokenizes templates" $ do
        it "tokenizes template containing booleans" $
            tokensOf (runLexer Tmp.tempBool) `shouldBe` tokensBool
        it "tokenizes template containing nil" $
            tokensOf (runLexer Tmp.tempNil) `shouldBe` tokensNil
        it "tokenizes template containing literals" $
            tokensOf (runLexer Tmp.tempLit) `shouldBe` tokensLit
        it "tokenizes template containing only comment" $
            tokensOf (runLexer Tmp.tempComment1) `shouldBe` tokensComment1
        it "tokenizes template containing comment" $
            tokensOf (runLexer Tmp.tempComment2) `shouldBe` tokensComment2
        it "tokenizes template containing if" $
            tokensOf (runLexer Tmp.tempIf) `shouldBe` tokensIf
        it "tokenizes template containing unless" $
            tokensOf (runLexer Tmp.tempUnless) `shouldBe` tokensUnless
        it "tokenizes template containing with" $
            tokensOf (runLexer Tmp.tempWith) `shouldBe` tokensWith
        it "tokenizes template containing arithmetic" $
            tokensOf (runLexer Tmp.tempArith) `shouldBe` tokensArith
    describe "detects lexer errors" $ do
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
        it "returns error for unclosed comment block" $
            runLexer unclosedTest4 `shouldBe` Left unclosedTest4Err
        it "returns error for unclosed string literal" $
            runLexer unclosedTest5 `shouldBe` Left unclosedTest5Err
        it "returns error for redundant section block closing tag" $
            runLexer noBlockToCloseTest1 `shouldBe` Left noBlockToCloseTest1Err
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
tokensOf (Right ls) = [l | Lexeme _ l _ <- ls]

-- | Expected tokens for `tempBool`.
tokensBool :: [LexemeClass]
tokensBool =
    [ LText "This is "
    , LLMustache
    , LIdentifier "true"
    , LRMustache
    , LText ". That is "
    , LLMustache
    , LIdentifier "false"
    , LRMustache
    , LText "."
    , LEOF
    ]

-- | Expected tokens for `tempNil`.
tokensNil :: [LexemeClass]
tokensNil =
    [ LText "Nothing is here, except "
    , LLMustache
    , LIdentifier "nil"
    , LRMustache
    , LText "."
    , LEOF
    ]

-- | Expected tokens for `tempLit`.
tokensLit :: [LexemeClass]
tokensLit =
    [ LText "First "
    , LLMustache
    , LInteger 10
    , LRMustache
    , LText " digits of "
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

-- | Expected tokens for `tempComment1`.
tokensComment1 :: [LexemeClass]
tokensComment1 = [LEOF]

-- | Expected tokens for `tempComment2`.
tokensComment2 :: [LexemeClass]
tokensComment2 =
    [ LText "hello world."
    , LEOF
    ]

-- | Expected tokens for `tempIf`.
tokensIf :: [LexemeClass]
tokensIf =
    [ LLMustachePound
    , LIdentifier "if"
    , LLParen
    , LIdentifier "defined?"
    , LIdentifier "$name"
    , LRParen
    , LRMustache
    , LText "Hello "
    , LLMustache
    , LIdentifier "$name"
    , LRMustache
    , LText "."
    , LCloseMustachePound
    , LEOF
    ]

-- | Expected tokens for `tempUnless`.
tokensUnless :: [LexemeClass]
tokensUnless =
    [ LLMustachePound
    , LIdentifier "unless"
    , LLParen
    , LIdentifier "defined?"
    , LIdentifier "$name"
    , LRParen
    , LRMustache
    , LText "Hello world."
    , LCloseMustachePound
    , LEOF
    ]

-- | Expected tokens for `tempWith`.
tokensWith :: [LexemeClass]
tokensWith =
    [ LLMustachePound
    , LIdentifier "with"
    , LIdentifier "$list"
    , LRMustache
    , LLMustache
    , LIdentifier "$$element"
    , LRMustache
    , LText " and "
    , LCloseMustachePound
    , LEOF
    ]

-- | Expected tokens for `tempArith`.
tokensArith :: [LexemeClass]
tokensArith =
    [ LText "The answer is "
    , LLMustache
    , LIdentifier "+"
    , LLParen
    , LIdentifier "-"
    , LInteger 50
    , LInteger 20
    , LRParen
    , LInteger 12
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

-- | Sample template with unclosed comment block.
unclosedTest4 :: String
unclosedTest4 = "Hello {{! comment"

-- | Expected error for `unclosedTest4`.
unclosedTest4Err :: String
unclosedTest4Err = "Unclosed comment block at end of file"

-- | Sample template with unclosed string literal.
unclosedTest5 :: String
unclosedTest5 = "Hello {{ \"world"

-- | Expected error for `unclosedTest5`.
unclosedTest5Err :: String
unclosedTest5Err = "Unclosed string literal at end of file"

-- | Sample template with redundant section block closing tag.
noBlockToCloseTest1 :: String
noBlockToCloseTest1 = "Hello {{# name }}{{ name }}{{/#}}{{/#}}"

-- | Expected error for `noBlockToCloseTest1`.
noBlockToCloseTest1Err :: String
noBlockToCloseTest1Err = "No section block to close at 1:40 before end of file"

-- | Sample template with error in the middle of line.
locationTest1 :: String
locationTest1 = "Hello {{ \"world\\_\" }}."

-- | Expected error for `locationTest1`.
locationTest1Err :: String
locationTest1Err =
    "Unknown escape sequence '\\_' at 1:18 on character '_' before `\" }}.`"

-- | Sample template with error before end of line.
locationTest2 :: String
locationTest2 = "Hello {{ \"world\\_\nand goodbye."

-- | Expected error for `locationTest2`.
locationTest2Err :: String
locationTest2Err = "Unknown escape sequence '\\_' at 1:18 before end of line"

-- | Sample template with error before end of file.
locationTest3 :: String
locationTest3 = "Hello {{ \"world\\_"

-- | Expected error for `locationTest3`.
locationTest3Err :: String
locationTest3Err = "Unknown escape sequence '\\_' at 1:18 before end of file"
