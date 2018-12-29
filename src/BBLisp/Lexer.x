{
module BBLisp.Lexer
    (
      -- * Types
      Lexeme
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
@decimal           = $digit+.$digit+

state :-

<0>       "{{!"         { enterComment `andBegin` comment }
<0>       "{{#"         { enterLisp    `andBegin` lisp }
<0>       "{{"          { enterLisp    `andBegin` lisp }
<0>       .             ;
<0>       \n            { skip }
<comment> "}}"          { leaveComment `andBegin` initial }
<comment> .             ;
<comment> \n            { skip }
<lisp>    \n            { skip }
<lisp>    $whitespace+  ;
<lisp>    @identifier   { mkIdentifier }
<lisp>    @decimal      { mkDecimal }
<lisp>    @integer      { mkInteger }
<lisp>    "}}"          { leaveLisp `andBegin` initial }
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
    = SINITIAL
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
    , lexerLispValue     :: [LexemeClass]
      -- Used by parser phase
    , parserCollIdent    :: Map String Int
    , parserCurrentToken :: Lexeme
    , parserPos          :: Pos
    }

-- | Initial lexer state.
initial :: Int
initial = 0

-- | Initial lexer user state.
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
    { lexerState         = SINITIAL
    , lexerLispValue     = []
    , parserCollIdent    = Map.empty
    , parserCurrentToken = Lexeme undefined EOF Nothing
    , parserPos          = Nothing
    }

-- | Make lexeme from lexeme class.
mkL :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
mkL c (p, _, _, str) len = return $ Lexeme p c $ Just $ take len str

-- | Modify current lexer state.
setLexerState :: LexerState -> Alex ()
setLexerState v = Alex $ \s ->
    Right (s{ alex_ust=(alex_ust s){ lexerState = v } }, ())

-- | Enter comment state.
enterComment :: Action
enterComment input len = do
    setLexerState SCOMMENT
    skip input len

-- | Leave comment state.
leaveComment :: Action
leaveComment input len = do
    setLexerState SINITIAL
    skip input len

-- | Enter lisp state.
enterLisp :: Action
enterLisp input len = do
    setLexerState SLISP
    skip input len

-- | Leave lisp state.
leaveLisp :: Action
leaveLisp input len = do
    setLexerState SINITIAL
    skip input len

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

-- | Run the lexer to produce lexemes.
runLexer :: String -> Either String [Lexeme]
runLexer str =
    runAlex str go
  where
    go = do
        (t, e) <- alexComplementError alexMonadScan
        case (t, e) of
            (_, Just err)       -> lexerError err
            (Lexeme _ EOF _, _) -> return [t]
            (_, _)              -> return . (t:) =<< go
}
