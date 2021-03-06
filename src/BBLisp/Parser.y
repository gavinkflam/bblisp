{
{-# LANGUAGE OverloadedStrings #-}
module BBLisp.Parser
    (
      -- * Parsing
      runParser
    ) where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Lbs
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector

import BBLisp.LexemeClass (LexemeClass(..))
import BBLisp.Lexer (Alex, Lexeme(..), alexError', alexMonadScan', runAlex)
import BBLisp.SyntaxTree (BList(..))
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
    '{'       { Lexeme _ LLBrace _ }
    '}'       { Lexeme _ LRBrace _ }
    '['       { Lexeme _ LLBracket _ }
    ']'       { Lexeme _ LRBracket _ }
    text      { Lexeme _ (LText $$) _ }
    integer   { Lexeme _ (LInteger $$) _ }
    decimal   { Lexeme _ (LDecimal $$) _ }
    string    { Lexeme _ (LString $$) _ }
    true      { Lexeme _ (LIdentifier "true") _ }
    false     { Lexeme _ (LIdentifier "false") _ }
    nil       { Lexeme _ (LIdentifier "nil") _ }
    ident     { Lexeme _ (LIdentifier $$) _ }

%%

Prog
    : Tmp                             { $1 }
    | {- empty -}                     { BList [BSymbol "str"] }

Tmp
    : Tmp1                            { BList [BSymbol "str", $1] }
    | Tmp Tmp1                        { appendList $1 $2 }

STmp
    : Tmp1                            { $1 }
    | STmp Tmp1                       { appendSTmp $1 $2 }

Tmp1
    : text                            { BString $1 }
    | '{{' List '}}'                  { $2 }
    | '{{#' List '}}' STmp '{{/#}}'   { appendList $2 $4 }

List
    : List1                           { $1 }
    | List List1                      { appendList $1 $2 }

List1
    : Literal                         { $1 }
    | '(' List ')'                    { $2 }

Literal
    : integer                         { BInteger $1 }
    | decimal                         { BDecimal $1 }
    | string                          { BString  $1 }
    | true                            { BBoolean True }
    | false                           { BBoolean False }
    | nil                             { BNil }
    | ident                           { BSymbol  $1 }
    | '{' Dict '}'                    { $2 }
    | '[' Vector ']'                  { $2 }

Dict
    : string List1                    { BDict $ Map.singleton $1 $2 }
    | Dict string List1               { insertDict $2 $3 $1 }

Vector
    : List1                           { BVector $ Vector.singleton $1 }
    | Vector List1                    { appendVector $1 $2 }
{
-- | Wrapper of lexer.
lexer :: (Lexeme -> Alex a) -> Alex a
lexer = (alexMonadScan' >>=)

-- | Append `l2` into `l1`.
--
--   Make a new list if `l1` is not yet a list.
appendList :: BList -> BList -> BList
appendList (BList l1) l2 = BList $ l1 ++ [l2]
appendList l1 l2         = BList [l1, l2]

-- | Append `l2` into the `str` sequence.
--
--   Make a new `str` sequence if it is not yet a `str` sequence.
appendSTmp :: BList -> BList -> BList
appendSTmp (BList (BSymbol "str" : l1)) l2 = BList $ BSymbol "str" : l1 ++ [l2]
appendSTmp l1 l2                           = BList [BSymbol "str", l1, l2]

-- | Insert a new key and value in the dictionary.
insertDict :: Bs.ByteString -> BList -> BList -> BList
insertDict k v (BDict m) = BDict $ Map.insert k v m
insertDict _ _ _ = error "cannot insert into non-dictionary data type"

-- | Insert a new element in the vector.
appendVector :: BList -> BList -> BList
appendVector (BVector v) ele = BVector $ Vector.snoc v ele
appendVector _ _ = error "cannot insert into non-vector data type"

-- | Produce a parser error with readable error message and location
--   information.
happyError :: Lexeme -> Alex a
happyError (Lexeme _ l _) =
    alexError' $ "parse error at token '" ++ show l ++ "'"

-- | Run the parser to produce syntax tree.
runParser :: Lbs.ByteString -> Either String BList
runParser = flip runAlex parse
}
