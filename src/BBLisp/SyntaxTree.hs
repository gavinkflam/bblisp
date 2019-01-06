module BBLisp.SyntaxTree
    (
      -- * Types
      List(..)
    ) where

import Data.Scientific (Scientific)

data List
    = Boolean Bool
    | Integer Integer
    | Decimal Scientific
    | String  String
    | Symbol  String
    | List    [List]
    deriving (Eq, Show)
