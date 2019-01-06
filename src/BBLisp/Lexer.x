{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- ^ Generated template contains unused qualified import of Control.Monad.
--   This should be reviewed in the future.

module BBLisp.Lexer
    (
      -- * Types
      Lexeme(..)
      -- * Monads
    , AlexUserState
    , Alex
      -- * Lexing
    , alexError'
    , alexMonadScan'
    , runAlex
    ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Numeric (readDec)

import Data.Map (Map)
import qualified Data.Map as Map

import BBLisp.LexemeClass (LexemeClass(..))
}

%wrapper "monadUserState"

$whitespace        = [\ \t\b]
$textchar          = [$printable $white] # \{
$commentchar       = [$printable $white] # \}
$stringchar        = [$printable] # [\"\\]

$digit             = 0-9
$alpha             = [a-zA-Z]
$specialinitial    = [\$\%\&\*\+\-\:\<\=\>\?\_\~]
$specialsubsequent = [\!\$\%\&\*\+\-\.\/\:\<\=\>\?\@\^\_\~]

$initial           = [$alpha $specialinitial]
$subsequent        = [$alpha $digit $specialsubsequent]

@text              = $textchar+
@comment           = $commentchar+
@string            = $stringchar+

@identifier        = $initial$subsequent*
@integer           = $digit+
@decimal           = $digit+\.$digit+

state :-

<0>       "{{!"         { enterComment `andBegin` comment }
<0>       "{{#"         { enterLisp LLMustachePound `andBegin` lisp }
<0>       "{{/#}}"      { closeSectionBlock }
<0>       "{{"          { enterLisp LLMustache `andBegin` lisp }
<0>       @text         { addToText }
<comment> "}}"          { leaveComment `andBegin` template }
<comment> \}            ;
<comment> @comment      ;
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
<string>  @string       { addToString }
<string>  \n            { invalidMultilineString }

{
-- | Lexer action type.
type Action = AlexInput -> Int -> Alex Lexeme

-- | Optional position information.
type Pos    = Maybe AlexPosn

-- | Lexeme containing the position, token and text.
data Lexeme =
    Lexeme AlexPosn LexemeClass (Maybe String)
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
    , lexerSectionDepth  :: Integer
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
    , lexerSectionDepth  = 0
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

-- | Add the string to text value.
addStrToLexerTextValue :: String -> Alex ()
addStrToLexerTextValue str = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerTextValue = newVal s } }, ())
  where
    newVal s = lexerTextValue (alex_ust s) ++ str

-- | Clear text value.
clearLexerTextValue :: Alex ()
clearLexerTextValue = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerTextValue = "" } }, ())

-- | Get the string value.
getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{ alex_ust = ust } ->
    Right (s, lexerStringValue ust)

-- | Add the string to string value.
addStrToLexerStringValue :: String -> Alex ()
addStrToLexerStringValue str = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerStringValue = newVal s } }, ())
  where
    newVal s = lexerStringValue (alex_ust s) ++ str

-- | Clear string value.
clearLexerStringValue :: Alex ()
clearLexerStringValue = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerStringValue = "" } }, ())

-- | Get the current lexer section depth.
getLexerSectionDepth :: Alex Integer
getLexerSectionDepth = Alex $ \s@AlexState{ alex_ust = ust } ->
    Right (s, lexerSectionDepth ust)

-- | Update the current lexer section depth with a function.
updateLexerSectionDepth :: (Integer -> Integer) -> Alex ()
updateLexerSectionDepth f = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerSectionDepth = newVal s } }, ())
  where
    newVal s = f $ lexerSectionDepth $ alex_ust s

-- | Enter comment state.
enterComment :: Action
enterComment _ _ = setLexerState SComment >> alexMonadScan'

-- | Leave comment state.
leaveComment :: Action
leaveComment _ _ = setLexerState STemplate >> alexMonadScan'

-- | Common action for entering lisp state.
--
--   Text lexeme should be added if applicable.
enterLispCommon :: LexemeClass -> Action
enterLispCommon l input len = setLexerState SLisp >> mkL l input len

-- | Enter lisp state.
enterLisp :: LexemeClass -> Action
enterLisp l@LLMustache input len = enterLispCommon l input len
enterLisp l@LLMustachePound input len =
    updateLexerSectionDepth (flip (+) 1) >> enterLispCommon l input len
enterLisp l _ _ = error $ "Invalid call to enterLisp: " ++ show l

-- | Enter string state.
enterString :: Action
enterString _ _ = setLexerState SString >> alexMonadScan'

-- | Leave string state.
leaveString :: Action
leaveString input len =
    setLexerState SLisp >> getLexerStringValue >>= mkString
  where
    mkString s = mkL (LString s) input len

-- | Close section block.
closeSectionBlock :: Action
closeSectionBlock input len = do
    depth <- getLexerSectionDepth
    if depth > 0
        then updateLexerSectionDepth (flip (-) 1) >> mkCloseMustachePound
        else alexError' "No section block to close"
  where
    mkCloseMustachePound = mkL LCloseMustachePound input len

-- | Leave lisp state.
leaveLisp :: LexemeClass -> Action
leaveLisp l@LRMustache input len = setLexerState STemplate >> mkL l input len
leaveLisp l _ _ = error $ "Invalid call to leaveLisp: " ++ show l

-- | Add the current character to text value store.
--
--   Make the text lexeme if the following characters will end the text block.
addToText :: Action
addToText (p, _, _, str) len = do
    addStrToLexerTextValue $ take len str
    if isEndOfText $ drop len str
        then mkText <$> getAndClearLexerTextValue
        else alexMonadScan'
  where
    mkText s = Lexeme p (LText s) $ Just s
    getAndClearLexerTextValue = do
        s <- getLexerTextValue
        clearLexerTextValue
        return s

-- | Add character to string value.
addCharToString :: Char -> Action
addCharToString c _ _ = addStrToLexerStringValue [c] >> alexMonadScan'

-- | Add the current character to string value store.
addToString :: Action
addToString (_, _, _, str) len =
    addStrToLexerStringValue (take len str) >> alexMonadScan'

-- | Make lexeme from lexeme class.
mkL :: LexemeClass -> Action
mkL l (p, _, _, str) len = return $ Lexeme p l $ Just $ take len str

-- | Read and make identifier lexeme.
mkIdentifier :: Action
mkIdentifier (p, _, _, str) len =
    return $ Lexeme p (LIdentifier str') $ Just str'
  where
    str' = take len str

-- | Read and make integer lexeme.
mkInteger :: Action
mkInteger (p, _, _, str) len =
    case readDec str' of
        [(val, _)] -> return $ Lexeme p (LInteger val) $ Just str'
        _          -> alexError' "Invalid integer"
  where
    str' = take len str

-- | Read and make decimal lexeme.
mkDecimal :: Action
mkDecimal (p, _, _, str) len =
    case reads str' of
        [(val, _)] -> return $ Lexeme p (LDecimal val) $ Just str'
        _          -> alexError' "Invalid decimal"
  where
    str' = take len str

-- | Returns the longest prefix of the list `xs` that satisfy the predicate `p`,
--   plus the immediately next element if any.
takeWhileAndOneMore :: (a -> Bool) -> [a] -> ([a], Maybe a)
takeWhileAndOneMore p xs =
    uncurry f $ span p xs
  where
    f ys []    = (ys, Nothing)
    f ys (z:_) = (ys, Just z)

-- | Peek input string to see if the upcoming sequence will end the text.
--
-- End of file or an unescaped '{{' sequence will end the text.
--
-- '{{|' is the escaped mustache sequence while '{{!' is comment open tag thus
-- not ending the text.
isEndOfText :: String -> Bool
isEndOfText str =
    nextIsEof str || nextIsLMustache str
  where
    nextIsEof = null
    nextIsLMustache s =
        case takeWhileAndOneMore (== '{') s of
            ([],   _)        -> False
            ("{{", Just '!') -> False
            (_,    Just '|') -> False
            (_,    _)        -> True

-- | Return error for unknown escape sequence.
unknownEscapeSequence :: Action
unknownEscapeSequence (_, _, _, str) len =
    alexError' $ "Unknown escape sequence '" ++ take len str ++ "'"

-- | Return error for invalid multiline string literal.
invalidMultilineString :: Action
invalidMultilineString _ _ = alexError' "Invalid multiline string literal"

-- | EOF lexeme needed by Alex.
alexEOF :: Alex Lexeme
alexEOF = return $ Lexeme undefined LEOF Nothing

-- | Remove leading and trailing white space from a string.
strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

-- | Show line and column number in a string.
showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = concat [show line, ":", show col]

-- | Produce a lexer error with readable error message and location information.
alexError' :: String -> Alex a
alexError' msg = do
    (p, c, _, str) <- alexGetInput
    alexError $ concat [msg, " at ", showPosn p, locationMsg c str]
  where
    remainingLine s  = take 30 $ strip $ takeWhile (`notElem` "\r\n") s
    locationMsg _ "" = " before end of file"
    locationMsg c s  =
        case remainingLine s of
            "" -> " before end of line"
            m  -> concat [" on character ", show c, " before `", m, "`"]

-- | Modified `alexMonadScan` with state checking mechanism for end of file
--   and better error message.
alexMonadScan' :: Alex Lexeme
alexMonadScan' = do
    input <- alexGetInput
    code  <- alexGetStartCode
    case alexScan input code of
        AlexEOF -> leaveLexer
        AlexSkip input' _ -> alexSetInput input' >> alexMonadScan'
        AlexToken input' len f ->
            alexSetInput input' >> f (ignorePendingBytes input) len
        AlexError (_, _, _, c:_) ->
            alexError' $ "unexpected character '" ++ [c] ++ "'"
        AlexError (_, _, _, _) -> alexError' "unexpected lexer error"

-- | Finish lexing as EOF was encountered.
--
--   Make text lexeme or throw error for unfinished comment/lisp/string state
--   or unclosed mustache tag.
leaveLexer :: Alex Lexeme
leaveLexer =
    leaveState =<< getLexerState
  where
    unclosedErr t = alexError $ "Unclosed " ++ t ++ " at end of file"
    leaveState SComment  = unclosedErr "comment block"
    leaveState SLisp     = unclosedErr "code block"
    leaveState SString   = unclosedErr "string literal"
    leaveState STemplate = leaveTemplate =<< getLexerSectionDepth
    leaveTemplate 0 = alexEOF
    leaveTemplate _ = unclosedErr "section block"
}
