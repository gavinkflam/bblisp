module BBLisp.LexemeClass
    (
      -- * Types
      LexemeClass(..)
    ) where

import qualified Data.ByteString as Bs

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
    | LText       Bs.ByteString
    | LIdentifier Bs.ByteString
    | LString     Bs.ByteString
    | LBoolean    Bool
    | LInteger    Integer
    | LDecimal    Scientific
    deriving (Eq, Show)
