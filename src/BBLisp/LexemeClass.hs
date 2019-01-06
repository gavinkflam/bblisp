module BBLisp.LexemeClass
    (
      -- * Types
      LexemeClass(..)
    ) where

import Data.Scientific (Scientific)

-- | Lexeme tokens.
data LexemeClass
    = LEOF
    | LLMustache
    | LLMustachePound
    | LRMustache
    | LCloseMustachePound
    | LLParen
    | LRParen
    | LText       String
    | LIdentifier String
    | LString     String
    | LBoolean    Bool
    | LInteger    Integer
    | LDecimal    Scientific
    deriving (Eq, Show)
