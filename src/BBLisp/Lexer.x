{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- ^ Generated template contains unused qualified import of Data.Monad.
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
$specialinitial    = [\!\$\%\&\*\+\-\/\:\<\=\>\?\^\_\~]
$specialsubsequent = [\!\$\%\&\*\+\-\.\/\:\<\=\>\?\@\^\_\~]

$initial           = [$alpha $specialinitial]
$subsequent        = [$alpha $digit $specialsubsequent]

@identifier        = $initial$subsequent*
@integer           = $digit+
@decimal           = $digit+\.$digit+

state :-

<0>       "{{!"         { enterComment `andBegin` comment }
<0>       "{{#"         { enterLisp    `andBegin` lisp }
<0>       "{{"          { enterLisp    `andBegin` lisp }
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
<lisp>    "}}"          { leaveLisp `andBegin` template }
<lisp>    "("           { mkL LPAREN }
<lisp>    ")"           { mkL RPAREN }

{
-- | Lexer user state function type.
type Action = AlexInput -> Int -> Alex Lexeme

-- | Optional position information.
type Pos    = Maybe AlexPosn

-- | Lexeme containing the position, token and text.
data Lexeme = Lexeme AlexPosn LexemeClass (Maybe String)

-- | Lexeme tokens.
data LexemeClass
    = EOF
    | LPAREN
    | RPAREN
    | TEXT       String
    | IDENTIFIER String
    | STRING     String
    | BOOLEAN    Bool
    | INTEGER    Integer
    | DECIMAL    Scientific
    deriving (Eq, Show)

-- | Possible lexer states.
data LexerState
    = STEMPLATE
    | SCOMMENT
    | SLISP
    | SSTRING
    deriving (Eq, Show)

-- | Lexer user state container lexer state information and position
--   information for parser.
data AlexUserState = AlexUserState
    {
      -- Used by lexer phase
      lexerState         :: LexerState
    , lexerTextValue     :: String
    , lexerLispValue     :: [LexemeClass]
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
    { lexerState         = STEMPLATE
    , lexerTextValue     = ""
    , lexerLispValue     = []
    , parserCollIdent    = Map.empty
    , parserCurrentToken = Lexeme undefined EOF Nothing
    , parserPos          = Nothing
    }

-- | Make lexeme from lexeme class.
mkL :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
mkL c (p, _, _, str) len = return $ Lexeme p c $ Just $ take len str

-- | Get current lexer state.
getLexerState :: Alex LexerState
getLexerState = Alex $ \s@AlexState{ alex_ust = ust } ->
    Right (s, lexerState ust)

-- | Modify current lexer state.
setLexerState :: LexerState -> Alex ()
setLexerState v = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerState = v } }, ())

-- | Get the text value string.
getLexerTextValue :: Alex String
getLexerTextValue = Alex $ \s@AlexState{ alex_ust = ust } ->
    Right (s, lexerTextValue ust)

-- | Add the character to text value string.
addCharToLexerTextValue :: Char -> Alex ()
addCharToLexerTextValue c = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerTextValue = newVal s } }, ())
  where
    newVal s = c : lexerTextValue (alex_ust s)

-- | Clear text value string.
clearLexerTextValue :: Alex ()
clearLexerTextValue = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerTextValue = "" } }, ())

-- | Enter comment state.
enterComment :: Action
enterComment _ _ = setLexerState SCOMMENT >> alexMonadScan

-- | Leave comment state.
leaveComment :: Action
leaveComment _ _ = setLexerState STEMPLATE >> alexMonadScan

-- | Enter lisp state.
enterLisp :: Action
enterLisp input len = setLexerState SLISP >> mkText input len

-- | Leave lisp state.
leaveLisp :: Action
leaveLisp _ _ = setLexerState STEMPLATE >> alexMonadScan

-- | Add character to text value.
addCharToText :: Char -> Action
addCharToText c _ _ = addCharToLexerTextValue c >> alexMonadScan

-- | Add the current character to text value store.
addToText :: Action
addToText i@(_, _, _, c:_) 1 = addCharToText c i 1
addToText _ _                = error "Invalid call to addToText"

-- | Make the text lexeme if there is any string in text value.
mkText :: Action
mkText (p, _, _, str) len = do
    s <- getLexerTextValue
    case s of
        "" -> alexMonadScan
        _  -> do
            clearLexerTextValue
            return $ Lexeme p (TEXT $ reverse s) $ Just $ take len str

-- | Read and make identifier lexeme.
mkIdentifier :: Action
mkIdentifier (p, _, _, str) len =
    return $ Lexeme p (IDENTIFIER str') $ Just str'
  where
    str' = take len str

-- | Read and make integer lexeme.
mkInteger :: Action
mkInteger (p, _, _, str) len =
    case readDec str' of
        [(val, _)] -> return $ Lexeme p (INTEGER val) $ Just str'
        _          -> lexerError "Invalid integer"
  where
    str' = take len str

-- | Read and make decimal lexeme.
mkDecimal :: Action
mkDecimal (p, _, _, str) len =
    case reads str' of
        [(val, _)] -> return $ Lexeme p (DECIMAL val) $ Just str'
        _          -> lexerError "Invalid decimal"
  where
    str' = take len str

-- | EOF lexeme needed by Alex.
alexEOF :: Alex Lexeme
alexEOF = return $ Lexeme undefined EOF Nothing

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
    alexError $ concat [strip msg, " at ", showPosn p, friendlyMsg c str]
  where
    trimMsg s        = take 30 $ strip $ takeWhile (`elem` "\r\n") $ s
    friendlyMsg _ "" = " at end of file"
    friendlyMsg c s  =
        case trimMsg s of
            "" -> " before end of line"
            m  -> concat ["on char ", show c, " before: '", m, "'"]

-- | Capture the error message to complement it with position information.
alexComplementError :: Alex a -> Alex (a, Maybe String)
alexComplementError (Alex al) =
    Alex f
  where
    f s = case al s of
        Left msg      -> Right (s, (undefined, Just msg))
        Right (s', x) -> Right (s', (x, Nothing))

-- | Finish lexing as EOF was encountered.
--   Make text lexeme or throw error for unfinished comment/lisp/string state.
leaveLexer :: Lexeme -> Alex [Lexeme]
leaveLexer eof@(Lexeme p _ str) = do
    st <- getLexerState
    case st of
        SCOMMENT  -> alexError "Unfinished comment block at end of file"
        SLISP     -> alexError "Unfinished code block at end of file"
        SSTRING   -> alexError "Unfinished string literal at end of file"
        STEMPLATE -> do
            s <- getLexerTextValue
            case s of
                "" -> return [eof]
                _  -> return [Lexeme p (TEXT $ reverse s) str, eof]

-- | Run the lexer to produce lexemes.
runLexer :: String -> Either String [Lexeme]
runLexer str =
    runAlex str go
  where
    go = do
        (l, e) <- alexComplementError alexMonadScan
        case (l, e) of
            (_, Just err)       -> lexerError err
            (Lexeme _ EOF _, _) -> leaveLexer l
            (_, _)              -> return . (l:) =<< go
}
