{
module BBLisp.Parser
    (
      -- * Parser
      runParser
    ) where

import BBLisp.LexemeClass (LexemeClass(..))
import BBLisp.Lexer (Alex, Lexeme(..), alexError', alexMonadScan', runAlex)
import BBLisp.SyntaxTree (Datum(..), TemplateClass(..))
}

%name parse
%tokentype { Lexeme }
%monad { Alex }
%lexer { lexer } { Lexeme _ LEOF _ }
%error { happyError }

%token
    '{{'      { Lexeme _ LLMustache _ }
    '{{#'     { Lexeme _ LLMustachePound _ }
    '{{^'     { Lexeme _ LLMustacheCaret _ }
    '}}'      { Lexeme _ LRMustache _ }
    '{{/#}}'  { Lexeme _ LCloseMustachePound _ }
    '{{/^}}'  { Lexeme _ LCloseMustacheCaret _ }
    '('       { Lexeme _ LLParen _ }
    ')'       { Lexeme _ LRParen _ }
    text      { Lexeme _ (LText $$) _ }
    integer   { Lexeme _ (LInteger $$) _ }
    decimal   { Lexeme _ (LDecimal $$) _ }
    string    { Lexeme _ (LString $$) _ }
    ident     { Lexeme _ (LIdentifier $$) _ }

%%

Template
    : text                                    { Text $1 }
    | '{{' List '}}'                          { Print $2 }
    | '{{#' List '}}' TemplateList '{{/#}}'   { Section $2 $4 }
    | '{{^' List '}}' TemplateList '{{/^}}'   { InvertSection $2 $4 }

TemplateList
    : Template                                { $1 }
    | TemplateList Template                   { mkTemplate $1 $2 }

Datum
    : integer                                 { Integer $1 }
    | decimal                                 { Decimal $1 }
    | string                                  { String  $1 }
    | ident                                   { Symbol  $1 }

Element
    : Datum                                   { $1 }
    | '(' List ')'                            { $2 }

List
    : Element                                 { $1 }
    | List Element                            { mkList $1 $2 }
{
-- | Wrapper of lexer.
lexer :: (Lexeme -> Alex a) -> Alex a
lexer = (alexMonadScan' >>=)

-- | Join parallel template elements into template.
mkTemplate :: TemplateClass -> TemplateClass -> TemplateClass
mkTemplate (Template ts) t = Template (t : ts)
mkTemplate t1 t2           = Template [t2, t1]

-- | Join parallel datum into list.
mkList :: Datum -> Datum -> Datum
mkList (List ds) d = List (d : ds)
mkList d1 d2       = List [d2, d1]

-- | Produce a parser error with readable error message and location
--   information.
happyError :: Lexeme -> Alex a
happyError (Lexeme _ l _) =
    alexError' $ "parse error at token'" ++ show l ++ "'"

-- | Run the parser to produce syntax tree.
runParser :: String -> Either String TemplateClass
runParser = flip runAlex parse
}
