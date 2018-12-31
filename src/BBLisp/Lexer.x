{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- ^ Generated template contains unused qualified import of Control.Monad.
--   This should be reviewed in the future.

module BBLisp.Lexer
    (
      -- * Types
      Lexeme(..)
    , LexemeClass(..)
      -- * Monads
    , AlexUserState
      -- * Lexing
    , runLexer
    ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Numeric (readDec)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Scientific (Scientific)
}

%wrapper "monadUserState"

$whitespace        = [\ \t\b]
$digit             = 0-9
$alpha             = [a-zA-Z]
$specialinitial    = [\$\%\&\*\+\-\:\<\=\>\?\_\~]
$specialsubsequent = [\!\$\%\&\*\+\-\.\/\:\<\=\>\?\@\^\_\~]

$initial           = [$alpha $specialinitial]
$subsequent        = [$alpha $digit $specialsubsequent]

@identifier        = $initial$subsequent*
@integer           = $digit+
@decimal           = $digit+\.$digit+

state :-

<0>       "{{!"         { enterComment `andBegin` comment }
<0>       "{{#"         { enterLisp LLMustachePound `andBegin` lisp }
<0>       "{{^"         { enterLisp LLMustacheCaret `andBegin` lisp }
<0>       "{{/#}}"      { closeMustache LCloseMustachePound }
<0>       "{{/^}}"      { closeMustache LCloseMustacheCaret }
<0>       "{{"          { enterLisp LLMustache `andBegin` lisp }
<0>       .             { addToText }
<0>       \n            { addCharToText '\n' }
<comment> "}}"          { leaveComment `andBegin` template }
<comment> .             ;
<comment> \n            { skip }
<lisp>    \n            { skip }
<lisp>    $whitespace+  ;
<lisp>    @identifier   { mkIdentifier }
<lisp>    @decimal      { mkDecimal }
<lisp>    @integer      { mkInteger }
<lisp>    "}}"          { leaveLisp LRMustache `andBegin` template }
<lisp>    \"            { enterString `andBegin` string }
<lisp>    \(            { mkL LLParen }
<lisp>    \)            { mkL LRParen }
<string>  \\n           { addCharToString '\n' }
<string>  \\r           { addCharToString '\r' }
<string>  \\t           { addCharToString '\t' }
<string>  \\\"          { addCharToString '\"' }
<string>  \\\\          { addCharToString '\\' }
<string>  \\.           { unknownEscapeSequence }
<string>  \"            { leaveString `andBegin` lisp }
<string>  .             { addToString }
<string>  \n            { invalidMultilineString }

{
-- | Lexer action type.
type Action = AlexInput -> Int -> Alex [Lexeme]

-- | Optional position information.
type Pos    = Maybe AlexPosn

-- | Lexeme containing the position, token and text.
data Lexeme =
    Lexeme AlexPosn LexemeClass (Maybe String)
    deriving (Eq, Show)

-- | Lexeme tokens.
data LexemeClass
    = LEOF
    | LLMustache
    | LLMustachePound
    | LLMustacheCaret
    | LRMustache
    | LCloseMustachePound
    | LCloseMustacheCaret
    | LLParen
    | LRParen
    | LText       String
    | LIdentifier String
    | LString     String
    | LBoolean    Bool
    | LInteger    Integer
    | LDecimal    Scientific
    deriving (Eq, Show)

-- | Possible lexer states.
data LexerState
    = STemplate
    | SComment
    | SLisp
    | SString
    deriving (Eq, Show)

-- | Lexer user state container lexer state information and position
--   information for parser.
data AlexUserState = AlexUserState
    {
      -- Used by lexer phase
      lexerState         :: LexerState
    , lexerTextValue     :: String
    , lexerStringValue   :: String
    , lexerMustacheStack :: [LexemeClass]
      -- Used by parser phase
    , parserCollIdent    :: Map String Int
    , parserCurrentToken :: Lexeme
    , parserPos          :: Pos
    }

-- | Template lexer state.
template :: Int
template = 0

-- | Initial lexer user state.
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
    { lexerState         = STemplate
    , lexerTextValue     = ""
    , lexerStringValue   = ""
    , lexerMustacheStack = []
    , parserCollIdent    = Map.empty
    , parserCurrentToken = Lexeme undefined LEOF Nothing
    , parserPos          = Nothing
    }

-- | Get current lexer state.
getLexerState :: Alex LexerState
getLexerState = Alex $ \s@AlexState{ alex_ust = ust } ->
    Right (s, lexerState ust)

-- | Modify current lexer state.
setLexerState :: LexerState -> Alex ()
setLexerState v = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerState = v } }, ())

-- | Get the text value.
getLexerTextValue :: Alex String
getLexerTextValue = Alex $ \s@AlexState{ alex_ust = ust } ->
    Right (s, lexerTextValue ust)

-- | Add the character to text value.
addCharToLexerTextValue :: Char -> Alex ()
addCharToLexerTextValue c = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerTextValue = newVal s } }, ())
  where
    newVal s = c : lexerTextValue (alex_ust s)

-- | Clear text value.
clearLexerTextValue :: Alex ()
clearLexerTextValue = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerTextValue = "" } }, ())

-- | Get the string value.
getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{ alex_ust = ust } ->
    Right (s, lexerStringValue ust)

-- | Add the character to string value.
addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerStringValue = newVal s } }, ())
  where
    newVal s = c : lexerStringValue (alex_ust s)

-- | Clear string value.
clearLexerStringValue :: Alex ()
clearLexerStringValue = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerStringValue = "" } }, ())

-- | Get the top element of the mustache stack if any.
peekLexerMustacheStack :: Alex (Maybe LexemeClass)
peekLexerMustacheStack = Alex $ \s@AlexState{ alex_ust = ust } ->
    case lexerMustacheStack ust of
        []  -> Right (s, Nothing)
        l:_ -> Right (s, Just l)

-- | Remove the top element of the mustache stack if any.
popLexerMustacheStack :: Alex (Maybe LexemeClass)
popLexerMustacheStack = Alex $ \s@AlexState{ alex_ust = ust } ->
    case lexerMustacheStack ust of
        []   -> Right (s, Nothing)
        l:ls -> Right (fSetStack s ls, Just l)
  where
    fSetStack s ls = s{ alex_ust=(alex_ust s){ lexerMustacheStack = ls } }

-- | Add an element to the top of the mustache stack.
pushLexerMustacheStack :: LexemeClass -> Alex ()
pushLexerMustacheStack l = Alex $ \s@AlexState{ alex_ust = ust } ->
    Right (fSetStack s (l:lexerMustacheStack ust), ())
  where
    fSetStack s ls = s{ alex_ust=(alex_ust s){ lexerMustacheStack = ls } }

-- | Enter comment state.
enterComment :: Action
enterComment _ _ = setLexerState SComment >> alexMonadScan

-- | Leave comment state.
leaveComment :: Action
leaveComment _ _ = setLexerState STemplate >> alexMonadScan

-- | Common action for entering lisp state.
--
--   Text lexeme should be added if applicable.
enterLispCommon :: LexemeClass -> Action
enterLispCommon l input len =
    setLexerState SLisp >> mkTextEndingLexeme l input len

-- | Enter lisp state.
enterLisp :: LexemeClass -> Action
enterLisp l@LLMustache input len = enterLispCommon l input len
enterLisp l@LLMustachePound input len =
    pushLexerMustacheStack LLMustachePound >> enterLispCommon l input len
enterLisp l@LLMustacheCaret input len =
    pushLexerMustacheStack LLMustacheCaret >> enterLispCommon l input len
enterLisp l _ _ = error $ "Invalid call to enterLisp: " ++ show l

-- | Enter string state.
enterString :: Action
enterString _ _ = setLexerState SString >> alexMonadScan

-- | Leave string state.
leaveString :: Action
leaveString input len =
    setLexerState SLisp >> getLexerStringValue >>= mkString
  where
    mkString s = mkL (LString $ reverse s) input len

-- | Common action for closing mustache tags.
--
--   The closing tag should be checked against the mustache stack.
--
--   Text lexeme should be added if applicable.
--   Closing tag lexeme should be added as well.
closeMustacheCommon :: LexemeClass -> LexemeClass -> Action
closeMustacheCommon expect l input len = do
    top <- peekLexerMustacheStack
    if top == Just expect
        then popLexerMustacheStack >> mkTextEndingLexeme l input len
        else lexerError $ "Unmatched closing tag " ++ showTag l
  where
    showTag LCloseMustachePound = "{{/#}}"
    showTag LCloseMustacheCaret = "{{/^}}"
    showTag l'                  = error $ "Invalid call to showTag" ++ show l'

-- | Close mustache tag.
closeMustache :: LexemeClass -> Action
closeMustache l@LCloseMustachePound input len =
    closeMustacheCommon LLMustachePound l input len
closeMustache l@LCloseMustacheCaret input len =
    closeMustacheCommon LLMustacheCaret l input len
closeMustache l _ _ = error $ "Invalid call to closeMustache: " ++ show l

-- | Leave lisp state.
leaveLisp :: LexemeClass -> Action
leaveLisp l@LRMustache input len = setLexerState STemplate >> mkL l input len
leaveLisp l _ _ = error $ "Invalid call to leaveLisp: " ++ show l

-- | Add character to text value.
addCharToText :: Char -> Action
addCharToText c _ _ = addCharToLexerTextValue c >> alexMonadScan

-- | Add the current character to text value store.
addToText :: Action
addToText i@(_, _, _, c:_) 1 = addCharToText c i 1
addToText _ _                = error "Invalid call to addToText"

-- | Add character to string value.
addCharToString :: Char -> Action
addCharToString c _ _ = addCharToLexerStringValue c >> alexMonadScan

-- | Add the current character to string value store.
addToString :: Action
addToString i@(_, _, _, c:_) 1 = addCharToString c i 1
addToString _ _                = error "Invalid call to addToString"

-- | Make lexeme from lexeme class.
mkL :: LexemeClass -> Action
mkL l (p, _, _, str) len = return [Lexeme p l $ Just $ take len str]

-- | Make the text lexeme if there is any string in text value.
mkTextMaybe :: AlexInput -> Int -> Alex (Maybe Lexeme)
mkTextMaybe (p, _, _, str) len = do
    s <- getLexerTextValue
    case s of
        "" -> return Nothing
        _  -> return $ Just $ Lexeme p (LText $ reverse s) $ Just $ take len str

-- | Make lexeme from lexeme class.
--
--   Make the preceeding text lexeme and clear text value as well if applicable.
mkTextEndingLexeme :: LexemeClass -> Action
mkTextEndingLexeme l input len = do
    lLex <- mkL l input len
    text <- mkTextMaybe input len
    case text of
        Nothing -> return lLex
        Just t  -> clearLexerTextValue >> return (t:lLex)

-- | Read and make identifier lexeme.
mkIdentifier :: Action
mkIdentifier (p, _, _, str) len =
    return [Lexeme p (LIdentifier str') $ Just str']
  where
    str' = take len str

-- | Read and make integer lexeme.
mkInteger :: Action
mkInteger (p, _, _, str) len =
    case readDec str' of
        [(val, _)] -> return [Lexeme p (LInteger val) $ Just str']
        _          -> lexerError "Invalid integer"
  where
    str' = take len str

-- | Read and make decimal lexeme.
mkDecimal :: Action
mkDecimal (p, _, _, str) len =
    case reads str' of
        [(val, _)] -> return [Lexeme p (LDecimal val) $ Just str']
        _          -> lexerError "Invalid decimal"
  where
    str' = take len str

-- | Return error for unknown escape sequence.
unknownEscapeSequence :: Action
unknownEscapeSequence (_, _, _, str) len =
    lexerError $ "Unknown escape sequence '" ++ take len str ++ "'"

-- | Return error for invalid multiline string literal.
invalidMultilineString :: Action
invalidMultilineString _ _ = lexerError "Invalid multiline string literal"

-- | EOF lexeme needed by Alex.
alexEOF :: Alex [Lexeme]
alexEOF = return [Lexeme undefined LEOF Nothing]

-- | Remove leading and trailing white space from a string.
strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

-- | Show line and column number in a string.
showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = concat [show line, ":", show col]

-- | Produce a lexer error with readable error message and location information.
lexerError :: String -> Alex a
lexerError msg = do
    (p, c, _, str) <- alexGetInput
    alexError $ concat [msg, " at ", showPosn p, locationMsg c str]
  where
    remainingLine s  = take 30 $ strip $ takeWhile (`notElem` "\r\n") s
    locationMsg _ "" = " before end of file"
    locationMsg c s  =
        case remainingLine s of
            "" -> " before end of line"
            m  -> concat [" on character ", show c, " before `", m, "`"]

-- | Capture the error message to complement it with position information.
alexComplementError :: Alex a -> Alex (a, Maybe String)
alexComplementError (Alex al) =
    Alex f
  where
    f s = case al s of
        Left msg      -> Right (s, (undefined, Just msg))
        Right (s', x) -> Right (s', (x, Nothing))

-- | Finish lexing as EOF was encountered.
--
--   Make text lexeme or throw error for unfinished comment/lisp/string state
--   or unclosed mustache tag.
leaveLexer :: Lexeme -> Alex [Lexeme]
leaveLexer eof@(Lexeme p _ str) =
    leaveState =<< getLexerState
  where
    unclosedErr t = alexError $ "Unclosed " ++ t ++ " at end of file"
    leaveState SComment  = unclosedErr "comment block"
    leaveState SLisp     = unclosedErr "code block"
    leaveState SString   = unclosedErr "string literal"
    leaveState STemplate = leaveTemplate =<< peekLexerMustacheStack
    leaveTemplate (Just LLMustachePound) = unclosedErr "section block"
    leaveTemplate (Just LLMustacheCaret) = unclosedErr "invert section block"
    leaveTemplate (Just e)               = error $ "Invalid stack top" ++ show e
    leaveTemplate Nothing                = do
        s <- getLexerTextValue
        case s of
            "" -> return [eof]
            _  -> return [Lexeme p (LText $ reverse s) str, eof]

-- | Run the lexer to produce lexemes.
runLexer :: String -> Either String [Lexeme]
runLexer str =
    runAlex str go
  where
    go = do
        (l, e) <- alexComplementError alexMonadScan
        case (l, e) of
            (_, Just err)               -> alexError err
            ([l'@(Lexeme _ LEOF _)], _) -> leaveLexer l'
            (_, _)                      -> (l++) <$> go
}
