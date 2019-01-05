{
module BBLisp.Parser
    (
      -- * Parsing
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

Tmpl
    : Tmpl1                           { $1 }
    | Tmpl Tmpl1                      { mkTemplate $1 $2 }

Tmpl1
    : text                            { Text $1 }
    | '{{' List '}}'                  { Print $2 }
    | '{{#' List '}}' Tmpl '{{/#}}'   { Section $2 $4 }
    | '{{^' List '}}' Tmpl '{{/^}}'   { InvertSection $2 $4 }

List
    : Element                         { $1 }
    | List Element                    { mkList $1 $2 }

Element
    : Datum                           { $1 }
    | '(' List ')'                    { $2 }

Datum
    : integer                         { Integer $1 }
    | decimal                         { Decimal $1 }
    | string                          { String  $1 }
    | ident                           { Symbol  $1 }
{
-- | Wrapper of lexer.
lexer :: (Lexeme -> Alex a) -> Alex a
lexer = (alexMonadScan' >>=)

-- | Join parallel template classes into template.
mkTemplate :: TemplateClass -> TemplateClass -> TemplateClass
mkTemplate (Template ts) t = Template $ ts ++ [t]
mkTemplate t1 t2           = Template [t1, t2]

-- | Join parallel datum into list.
mkList :: Datum -> Datum -> Datum
mkList (List ds) d = List $ ds ++ [d]
mkList d1 d2       = List [d1, d2]

-- | Produce a parser error with readable error message and location
--   information.
happyError :: Lexeme -> Alex a
happyError (Lexeme _ l _) =
    alexError' $ "parse error at token '" ++ show l ++ "'"

-- | Run the parser to produce syntax tree.
runParser :: String -> Either String TemplateClass
runParser = flip runAlex parse
}
