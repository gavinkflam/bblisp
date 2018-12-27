{
module BBLisp.Lexer
    (
      -- * Types
      Lexeme
    , LexemeClass(..)
      -- * Monads
    , AlexUserState
    ) where

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

@symbol            = $initial$subsequent*
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
<lisp>    @symbol       { mkSymbol }
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
    | TEXT    String
    | SYMBOL  String
    | STRING  String
    | BOOLEAN Bool
    | INTEGER Integer
    | DECIMAL Scientific
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

-- | Read and make symbol lexeme.
mkSymbol :: Action
mkSymbol (p, _, _, str) len =
    return $ Lexeme p (SYMBOL str') $ Just str'
  where
    str' = take len str

-- | EOF lexeme needed by Alex.
alexEOF :: Alex Lexeme
alexEOF = return $ Lexeme undefined EOF Nothing
}
