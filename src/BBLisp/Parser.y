{
module BBLisp.Parser
    (
      -- * Parsing
      runParser
    ) where

import BBLisp.LexemeClass (LexemeClass(..))
import BBLisp.Lexer (Alex, Lexeme(..), alexError', alexMonadScan', runAlex)
import BBLisp.SyntaxTree (List(..))
}

%name parse
%tokentype { Lexeme }
%monad { Alex }
%lexer { lexer } { Lexeme _ LEOF _ }
%error { happyError }

%token
    '{{'      { Lexeme _ LLMustache _ }
    '{{#'     { Lexeme _ LLMustachePound _ }
    '}}'      { Lexeme _ LRMustache _ }
    '{{/#}}'  { Lexeme _ LCloseMustachePound _ }
    '('       { Lexeme _ LLParen _ }
    ')'       { Lexeme _ LRParen _ }
    text      { Lexeme _ (LText $$) _ }
    integer   { Lexeme _ (LInteger $$) _ }
    decimal   { Lexeme _ (LDecimal $$) _ }
    string    { Lexeme _ (LString $$) _ }
    ident     { Lexeme _ (LIdentifier $$) _ }

%%

Tmp
    : Tmp1                            { List [Symbol "str", $1] }
    | Tmp Tmp1                        { appendList $1 $2 }

STmp
    : Tmp1                            { $1 }
    | STmp Tmp1                       { appendSTmp $1 $2 }

Tmp1
    : text                            { String $1 }
    | '{{' List '}}'                  { $2 }
    | '{{#' List '}}' STmp '{{/#}}'   { appendList $2 $4 }

List
    : List1                           { $1 }
    | List List1                      { appendList $1 $2 }

List1
    : Literal                         { $1 }
    | '(' List ')'                    { $2 }

Literal
    : integer                         { Integer $1 }
    | decimal                         { Decimal $1 }
    | string                          { String  $1 }
    | ident                           { Symbol  $1 }
{
-- | Wrapper of lexer.
lexer :: (Lexeme -> Alex a) -> Alex a
lexer = (alexMonadScan' >>=)

-- | Append `l2` into `l1`.
--
--   Make a new list if `l1` is not yet a list.
appendList :: List -> List -> List
appendList (List l1) l2 = List $ l1 ++ [l2]
appendList l1 l2        = List [l1, l2]

-- | Append `l2` into the `str` sequence.
--
--   Make a new `str` sequence if it is not yet a `str` sequence.
appendSTmp :: List -> List -> List
appendSTmp (List (Symbol "str" : l1)) l2 = List $ Symbol "str" : l1 ++ [l2]
appendSTmp l1 l2                         = List [Symbol "str", l1, l2]

-- | Produce a parser error with readable error message and location
--   information.
happyError :: Lexeme -> Alex a
happyError (Lexeme _ l _) =
    alexError' $ "parse error at token '" ++ show l ++ "'"

-- | Run the parser to produce syntax tree.
runParser :: String -> Either String List
runParser = flip runAlex parse
}
