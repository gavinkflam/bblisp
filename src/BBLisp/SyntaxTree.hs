module BBLisp.SyntaxTree
    (
      -- * Types
      Bindings
    , Syntax
    , Function
    , List(..)
    , Primitive(..)
    ) where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as Bsc
import Data.Map.Strict (Map)

import Data.Scientific (Scientific)

-- | Alias for name to list bindings.
type Bindings = Map Bs.ByteString List

-- | Signature for syntactic form.
type Syntax   = Bindings -> [List] -> Either String (Bindings, List)

-- | Signature for pure function.
type Function = [List] -> Either String List

-- | Recursive list structure to store code and data.
data List
    = Boolean   Bool
    | Integer   Integer
    | Decimal   Scientific
    | String    Bs.ByteString
    | Symbol    Bs.ByteString
    | Nil
    | Primitive Primitive
    | List      [List]
    deriving (Eq, Show)

-- | Primitive syntactic form and pure function.
data Primitive
    = Syntax   Bs.ByteString Syntax
    | Function Bs.ByteString Function

instance Eq Primitive where
    (==) (Syntax n1 _)   (Syntax n2 _)   = n1 == n2
    (==) (Function n1 _) (Function n2 _) = n1 == n2
    (==) _                _              = False

instance Show Primitive where
    show (Syntax n _)   = "<# syntax " ++ Bsc.unpack n ++ ">"
    show (Function n _) = "<# function " ++ Bsc.unpack n ++ ">"
