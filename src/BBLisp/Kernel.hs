{-# LANGUAGE OverloadedStrings #-}

module BBLisp.Kernel
    (
      -- * Syntactic Forms
      eval
    , if'
      -- * Functions
    , str
      -- * Bindings
    , bindings
    ) where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as Bsc
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector

import BBLisp.SyntaxTree (Bindings, Function, List(..), Primitive(..), Syntax)

-- | Evaluate an expression or definition.
eval :: Syntax
eval b [v@Nil]         = Right (b, v)
eval b [v@(Boolean _)] = Right (b, v)
eval b [v@(Integer _)] = Right (b, v)
eval b [v@(Decimal _)] = Right (b, v)
eval b [v@(String _)]  = Right (b, v)
eval b [v@(Dict _)]    = Right (b, v)
eval b [v@(Vector _)]  = Right (b, v)
eval b [Symbol name]   =
    case b Map.!? name of
        Nothing -> Left $ "Binding for '" ++ Bsc.unpack name ++ "' not found"
        Just v  -> Right (b, v)
eval b [List [Dict map', String key]]  =
    case map' Map.!? key of
        Nothing -> Right (b, Nil)
        Just v  -> Right (b, v)
eval _ [List [Dict _, _]] = Left "Incorrect type for key"
eval b [List [Vector vector, Integer index]]  =
    case vector Vector.!? fromIntegral index of
        Nothing -> Right (b, Nil)
        Just v  -> Right (b, v)
eval _ [List [Vector _, _]] = Left "Incorrect type for index"
eval b [l@(List (s@(Symbol _) : ls))] =
    case eval b [s] of
        Left err                   -> Left err
        Right (_, p@(Primitive _)) -> eval b [List (p : ls)]
        Right _                    -> Left $ "Unexpected form: " ++ show l
eval b [List (Primitive (Syntax _ f) : ls)]   = f b ls
eval b [List (Primitive (Function _ f) : ls)] =
    case map snd <$> mapM (eval b . (:[])) ls of
        Left err -> Left err
        Right vs -> (,) b <$> f vs
eval _ l = Left $ "Unexpected form " ++ show l

-- | Evaluates `test`.
--
--   If it produces `true`, evaluate `then` and returns the result.
--   If it produces `false`, evaluate `else` and returns the result, or returns
--   `nil` when there are no `else`.
if' :: Syntax
if' b [test, then', else'] =
    case eval b [test] of
        Left  err                -> Left err
        Right (_, Boolean True)  -> eval b [then']
        Right (_, Boolean False) -> eval b [else']
        Right (_, _)             -> Left "Incorrect type for `test`."
if' b [test, then'] = if' b [test, then', Nil]
if' _ []            = Left "Too few arguments to if"
if' _ [_]           = Left "Too few arguments to if"
if' _ (_:_:_:_)     = Left "Too many arguments to if"

-- | With one argument, returns the string representation of `v`.
--
--   With more than one argument, returns the concatenation of the string
--   representations of each element of `vs`.
str :: Function
str []              = Right $ String Bsc.empty
str [Boolean True]  = Right $ String "true"
str [Boolean False] = Right $ String "false"
str [Integer v]     = Right $ String $ Bsc.pack $ show v
str [Decimal v]     = Right $ String $ Bsc.pack $ show v
str [s@(String _)]  = Right s
str [Symbol v]      = Right $ String v
str [Nil]           = Right $ String Bsc.empty
str [Primitive (Syntax   name _)] = Right $ String name
str [Primitive (Function name _)] = Right $ String name
str vs =
    String . Bs.concat . fStrs <$> mapM (str . (:[])) vs
  where
    fStrs ls = [ s | String s <- ls ]

-- | All primitives in the module.
bindings :: Bindings
bindings = Map.fromList
    [ ("eval",        Primitive $ Syntax "eval" eval)
    , ("if",          Primitive $ Syntax "if" if')
    , ("str",         Primitive $ Function "str" str)
    ]
