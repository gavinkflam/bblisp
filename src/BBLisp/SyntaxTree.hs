module BBLisp.SyntaxTree
    (
      -- * Types
      Binding
    , Syntax
    , Function
    , List(..)
    , Primitive(..)
    ) where

import Data.Map.Strict (Map)
import Data.Scientific (Scientific)

-- | Alias for name to list binding.
type Binding = Map String List

-- | Signature for syntactic form.
type Syntax   = Binding -> List -> Either String (Binding, List)

-- | Signature for pure function.
type Function = List -> Either String List

-- | Recursive list structure to store code and data.
data List
    = Boolean   Bool
    | Integer   Integer
    | Decimal   Scientific
    | String    String
    | Symbol    String
    | Primitive Primitive
    | List      [List]
    deriving (Eq, Show)

-- | Primitive syntactic form and pure function.
data Primitive
    = Syntax   String Syntax
    | Function String Function

instance Eq Primitive where
    (==) (Syntax n1 _)   (Syntax n2 _)   = n1 == n2
    (==) (Function n1 _) (Function n2 _) = n1 == n2
    (==) _                _              = False

instance Show Primitive where
    show (Syntax n _)   = "<# syntax " ++ n ++ ">"
    show (Function n _) = "<# function " ++ n ++ ">"
