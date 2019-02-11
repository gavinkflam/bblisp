{-# OPTIONS_GHC -funbox-strict-fields #-}

module BBLisp.SyntaxTree
    (
      -- * Types
      BBindings
    , BSyntax
    , BFunction
    , BList(..)
    , BPrimitive(..)
    ) where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as Bsc
import Data.Map.Strict (Map)
import Data.Vector (Vector)

import Data.Scientific (Scientific)

-- | Alias for name to list bindings.
type BBindings = Map Bs.ByteString BList

-- | Signature for syntactic form.
type BSyntax   = BBindings -> [BList] -> Either String (BBindings, BList)

-- | Signature for pure function.
type BFunction = [BList] -> Either String BList

-- | Recursive list structure to store code and data.
data BList
    = BBoolean   !Bool
    | BInteger   !Integer
    | BDecimal   !Scientific
    | BString    !Bs.ByteString
    | BSymbol    !Bs.ByteString
    | BDict      !(Map Bs.ByteString BList)
    | BNil
    | BPrimitive !BPrimitive
    | BList      ![BList]
    | BVector    !(Vector BList)
    deriving (Eq, Show)

-- | Primitive syntactic form and pure function.
data BPrimitive
    = BSyntax   !Bs.ByteString !BSyntax
    | BFunction !Bs.ByteString !BFunction

instance Eq BPrimitive where
    (==) (BSyntax n1 _)   (BSyntax n2 _)   = n1 == n2
    (==) (BFunction n1 _) (BFunction n2 _) = n1 == n2
    (==) _                _                = False

instance Show BPrimitive where
    show (BSyntax n _)   = "<# syntax " ++ Bsc.unpack n ++ ">"
    show (BFunction n _) = "<# function " ++ Bsc.unpack n ++ ">"
