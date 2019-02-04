{
{-# LANGUAGE OverloadedStrings, ViewPatterns, PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -funbox-strict-fields #-}
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

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Builder as Bsb
import qualified Data.ByteString.Lazy as Lbs
import qualified Data.ByteString.Lazy.Char8 as Lbsc
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import BBLisp.LexemeClass (LexemeClass(..))
}

%wrapper "monadUserState-bytestring"

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
-- | Pattern synonyms for matching head of ByteString as characters.
infixr 5 :<
pattern (:<) :: Char -> Lbs.ByteString -> Lbs.ByteString
pattern b :< bs <- (Lbsc.uncons -> Just (b, bs))

-- | Pattern synonyms for matching empty ByteString.
pattern Empty :: Lbs.ByteString
pattern Empty <- (Lbs.uncons -> Nothing)

-- | Lexer action type.
type Action = AlexInput -> Int64 -> Alex Lexeme

-- | Optional position information.
type Pos    = Maybe AlexPosn

-- | Lexeme containing the position, token and text.
data Lexeme =
    Lexeme !AlexPosn !LexemeClass !(Maybe Lbs.ByteString)
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
      lexerState         :: !LexerState
    , lexerTextValue     :: !Bsb.Builder
    , lexerStringValue   :: !Bsb.Builder
    , lexerSectionDepth  :: !Integer
      -- Used by parser phase
    , parserCollIdent    :: !(Map String Int)
    , parserCurrentToken :: !Lexeme
    , parserPos          :: !Pos
    }

-- | Template lexer state.
template :: Int
template = 0

-- | Initial lexer user state.
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
    { lexerState         = STemplate
    , lexerTextValue     = mempty
    , lexerStringValue   = mempty
    , lexerSectionDepth  = 0
    , parserCollIdent    = Map.empty
    , parserCurrentToken = Lexeme alexStartPos LEOF Nothing
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
getLexerTextValue :: Alex Bsb.Builder
getLexerTextValue = Alex $ \s@AlexState{ alex_ust = ust } ->
    Right (s, lexerTextValue ust)

-- | Update the text value with a function.
updateLexerTextValue :: (Bsb.Builder -> Bsb.Builder) -> Alex ()
updateLexerTextValue f = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerTextValue = newVal s } }, ())
  where
    newVal s = f $ lexerTextValue $ alex_ust s

-- | Get the string value.
getLexerStringValue :: Alex Bsb.Builder
getLexerStringValue = Alex $ \s@AlexState{ alex_ust = ust } ->
    Right (s, lexerStringValue ust)

-- | Update the string value with a function.
updateLexerStringValue :: (Bsb.Builder -> Bsb.Builder) -> Alex ()
updateLexerStringValue f = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerStringValue = newVal s } }, ())
  where
    newVal s = f $ lexerStringValue $ alex_ust s

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
    updateLexerSectionDepth (+ 1) >> enterLispCommon l input len
enterLisp l _ _ = error $ "Invalid call to enterLisp: " ++ show l

-- | Enter string state.
enterString :: Action
enterString _ _ = setLexerState SString >> alexMonadScan'

-- | Leave string state.
leaveString :: Action
leaveString input len =
    setLexerState SLisp >> getAndClearLexerStringValue >>= mkString
  where
    mkString s = mkL (LString $ builderToBS s) input len
    getAndClearLexerStringValue = do
        s <- getLexerStringValue
        updateLexerStringValue $ const mempty
        return s

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
addToText (p, _, str, _) len = do
    updateLexerTextValue (<> Bsb.lazyByteString (Lbs.take len str))
    if isEndOfText $ Lbs.drop len str
        then mkText <$> getAndClearLexerTextValue
        else alexMonadScan'
  where
    mkText s = Lexeme p (LText $ builderToBS s) $ Just $ Bsb.toLazyByteString s
    getAndClearLexerTextValue = do
        s <- getLexerTextValue
        updateLexerTextValue $ const mempty
        return s

-- | Add character to string value.
addCharToString :: Char -> Action
addCharToString c _ _ =
    updateLexerStringValue (<> Bsb.charUtf8 c) >> alexMonadScan'

-- | Add the current character to string value store.
addToString :: Action
addToString (_, _, str, _) len =
    updateLexerStringValue (<> Bsb.lazyByteString str') >> alexMonadScan'
  where
    str' = Lbs.take len str

-- | Make lexeme from lexeme class.
mkL :: LexemeClass -> Action
mkL l (p, _, str, _) len =
    return $ Lexeme p l $ Just $ Lbs.take len str

-- | Read and make identifier lexeme.
mkIdentifier :: Action
mkIdentifier (p, _, str, _) len =
    return $ Lexeme p (LIdentifier $ Lbs.toStrict str') $ Just str'
  where
    str' = Lbs.take len str

-- | Read and make integer lexeme.
mkInteger :: Action
mkInteger (p, _, str, _) len =
    case Lbsc.readInteger str' of
        Just (val, _) -> return $ Lexeme p (LInteger val) $ Just str'
        _             -> alexError' "Invalid integer"
  where
    str' = Lbs.take len str

-- | Read and make decimal lexeme.
mkDecimal :: Action
mkDecimal (p, _, str, _) len =
    case reads $ Lbsc.unpack str' of
        [(val, _)] -> return $ Lexeme p (LDecimal val) $ Just str'
        _          -> alexError' "Invalid decimal"
  where
    str' = Lbs.take len str

-- | Execute Builder and return the generated strict ByteString.
builderToBS :: Bsb.Builder -> Bs.ByteString
builderToBS = Lbs.toStrict . Bsb.toLazyByteString

-- | Returns the longest prefix of the list `xs` that satisfy the predicate `p`,
--   plus the immediately next element if any.
takeWhileAndOneMore
    :: (Char -> Bool) -> Lbs.ByteString -> (Lbs.ByteString, Maybe Char)
takeWhileAndOneMore p xs =
    uncurry f $ Lbsc.span p xs
  where
    f ys (z :< _) = (ys, Just z)
    f ys _        = (ys, Nothing)

-- | Peek input string to see if the upcoming sequence will end the text.
--
-- End of file or an unescaped '{{' sequence will end the text.
--
-- '{{|' is the escaped mustache sequence while '{{!' is comment open tag thus
-- not ending the text.
isEndOfText :: Lbs.ByteString -> Bool
isEndOfText str =
    nextIsEof str || nextIsLMustache str
  where
    nextIsEof = Lbs.null
    nextIsLMustache s =
        case takeWhileAndOneMore (== '{') s of
            (Empty,               _)        -> False
            ('{' :< '{' :< Empty, Just '!') -> False
            (_,                   Just '|') -> False
            _                               -> True

-- | Return error for unknown escape sequence.
unknownEscapeSequence :: Action
unknownEscapeSequence (_, _, str, _) len =
    alexError' $ concat ["Unknown escape sequence '", Lbsc.unpack str', "'"]
  where
    str' = Lbs.take len str

-- | Return error for invalid multiline string literal.
invalidMultilineString :: Action
invalidMultilineString _ _ = alexError' "Invalid multiline string literal"

-- | EOF lexeme needed by Alex.
alexEOF :: Alex Lexeme
alexEOF = Alex $ \s@AlexState{ alex_pos = pos } ->
    Right (s, Lexeme pos LEOF Nothing)

-- | Remove leading and trailing white space from a string.
strip :: Lbs.ByteString -> Lbs.ByteString
strip =
    dropWhileEnd isSpace . Lbsc.dropWhile isSpace
  where
    dropWhileEnd p bs   = fst $ Lbsc.foldr (acc p) (Lbs.empty, False) bs
    acc p c (bs, False) = if p c then (bs, False) else (Lbsc.cons c bs, True) 
    acc _ c (bs, True)  = (Lbsc.cons c bs, True) 

-- | Show line and column number in a string.
showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = concat [show line, ":", show col]

-- | Produce a lexer error with readable error message and location information.
alexError' :: String -> Alex a
alexError' msg = do
    (p, c, str, _) <- alexGetInput
    alexError $ concat [msg, " at ", showPosn p, locationMsg c str]
  where
    takeOneLine     = Lbsc.takeWhile (\c -> c /= '\r' && c /= '\n')
    remainingLine   = Lbs.take 30 . strip . takeOneLine
    locationMsg _ Empty = " before end of file"
    locationMsg c s     =
        case remainingLine s of
            Empty -> " before end of line"
            m     ->
                concat
                [" on character ", show c, " before `", Lbsc.unpack m, "`"]

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
            alexSetInput input' >>
            f (ignorePendingBytes input) (fromIntegral len)
        AlexError (_, _, c :< _, _) ->
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
