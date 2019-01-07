module BBLisp.Kernel
    (
      -- * Syntactic Forms
      eval
      -- * Functions
    , str
      -- * Primitives
    , primitives
    ) where

import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map

import BBLisp.SyntaxTree (Binding, Function, List(..), Primitive(..), Syntax)

-- | Evaluate an expression or definition.
eval :: Syntax
eval b l@(List (s@(Symbol _) : ls)) =
    case eval b s of
        Left err                   -> Left err
        Right (_, p@(Primitive _)) -> eval b $ List (p : ls)
        Right _                    -> Left $ "Unexpected form: " ++ show l
eval b (List (Primitive (Syntax _ f) : ls))   =
    case f b $ List ls of
        Left err      -> Left err
        Right (b', l) -> Right (b', l)
eval b (List (Primitive (Function _ f) : ls)) =
    case map snd <$> mapM (eval b) ls of
        Left err -> Left err
        Right vs -> (,) b <$> f (List vs)
eval b v@(Boolean _) = Right (b, v)
eval b v@(Integer _) = Right (b, v)
eval b v@(Decimal _) = Right (b, v)
eval b v@(String _)  = Right (b, v)
eval b (Symbol name) =
    case b !? name of
        Nothing -> Left $ "Binding '" ++ name ++ "' not found"
        Just v  -> Right (b, v)
eval _ l = Left $ "Unexpected form: " ++ show l

-- | With one argument, returns the string representation of `v`.
--
--   With more than one argument, returns the concatenation of the string
--   representation of `vs`.
str :: Function
str (List [Boolean True])  = Right $ String "true"
str (List [Boolean False]) = Right $ String "false"
str (List [Integer v])     = Right $ String $ show v
str (List [Decimal v])     = Right $ String $ show v
str (List [s@(String _)])  = Right s
str (List [Symbol v])      = Right $ String v
str (List [Primitive (Syntax   name _)]) = Right $ String name
str (List [Primitive (Function name _)]) = Right $ String name
str (List vs) =
    String . concat . fStrs <$> mapM (str . List . (:[])) vs
  where
    fStrs ls = [ s | String s <- ls ]
str l = Left $ "Unexpected form: " ++ show l

-- | Binding of all primitives in the module.
primitives :: Binding
primitives = Map.fromList
    [ ("eval",        Primitive $ Syntax "eval" eval)
    , ("str",         Primitive $ Function "str" str)
    ]
