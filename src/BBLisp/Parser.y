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

List
    : List1                           { $1 }
    | List List1                      { joinList $1 $2 }

List1
    : text                            { mkPrint $ String $1 }
    | '{{' List '}}'                  { mkPrint $2 }
    | '{{#' List '}}' List '{{/#}}'   { mkSection $2 $4 }
    | Literal                         { $1 }
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

-- | Join parallel list into a single list.
joinList :: List -> List -> List
joinList (List [Symbol "block", List l1]) l2 =
    List [Symbol "block", List $ l1 ++ [l2]]
joinList l1@(List _) l2@(List _) = List [Symbol "block", List [l1, l2]]
joinList (List l1) l2            = List $ l1 ++ [l2]
joinList l1 l2                   = List [l1, l2]

-- | Make a list for section sequence.
mkSection :: List -> List -> List
mkSection (List l1) l2 = List $ l1 ++ [l2]
mkSection l1 l2        = List [l1, l2]

-- | Make a list for print sequence.
mkPrint :: List -> List
mkPrint d = List [Symbol "print", d]

-- | Produce a parser error with readable error message and location
--   information.
happyError :: Lexeme -> Alex a
happyError (Lexeme _ l _) =
    alexError' $ "parse error at token '" ++ show l ++ "'"

-- | Run the parser to produce syntax tree.
runParser :: String -> Either String List
runParser = flip runAlex parse
}
