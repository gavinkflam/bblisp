module BBLisp.SyntaxTree
    (
      -- * Types
      TemplateClass(..)
    , Datum(..)
    ) where

import Data.Scientific (Scientific)

data TemplateClass
    = Text          String
    | Print         Datum
    | Section       Datum TemplateClass
    | InvertSection Datum TemplateClass
    | Template      [TemplateClass]
    deriving (Eq, Show)

data Datum
    = Boolean Bool
    | Integer Integer
    | Decimal Scientific
    | String  String
    | Symbol  String
    | List    [Datum]
    deriving (Eq, Show)
