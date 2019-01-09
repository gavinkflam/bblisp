module BBLisp.Kernel
    (
      -- * Syntactic Forms
      eval
    , if'
      -- * Functions
    , str
      -- * Primitives
    , primitives
    ) where

import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map

import BBLisp.SyntaxTree (Bindings, Function, List(..), Primitive(..), Syntax)

-- | Evaluate an expression or definition.
eval :: Syntax
eval b [v@(Boolean _)] = Right (b, v)
eval b [v@(Integer _)] = Right (b, v)
eval b [v@(Decimal _)] = Right (b, v)
eval b [v@(String _)]  = Right (b, v)
eval b [Symbol name]   =
    case b !? name of
        Nothing -> Left $ "Binding for '" ++ name ++ "' not found"
        Just v  -> Right (b, v)
eval b l@(s@(Symbol _) : ls) =
    case eval b [s] of
        Left err                   -> Left err
        Right (_, p@(Primitive _)) -> eval b (p : ls)
        Right _                    -> Left $ "Unexpected form: " ++ show l
eval b (Primitive (Syntax _ f) : ls)   = f b ls
eval b (Primitive (Function _ f) : ls) =
    case map snd <$> mapM (eval b . (:[])) ls of
        Left err -> Left err
        Right vs -> (,) b <$> f vs
eval _ l = Left $ "eval: unexpected form " ++ show l

-- | Evaluates `test`.
--
--   If it produces any value other than `false`, evaluate `then` and returns
--   the result.
--
--   Otherwise, evaluate `else` and returns the result, or returns `nil` when
--   there are no `else`.
if' :: Syntax
if' b [test, then', else'] =
    case eval b [test] of
        Left  err                -> Left err
        Right (_, Boolean False) -> eval b [else']
        Right _                  -> eval b [then']
if' _ l = Left $ "if: Unexpected form " ++ show l
-- ^ TODO: Recursively define pattern without `else` after adding `nil`.

-- | With one argument, returns the string representation of `v`.
--
--   With more than one argument, returns the concatenation of the string
--   representation of `vs`.
str :: Function
str [Boolean True]  = Right $ String "true"
str [Boolean False] = Right $ String "false"
str [Integer v]     = Right $ String $ show v
str [Decimal v]     = Right $ String $ show v
str [s@(String _)]  = Right s
str [Symbol v]      = Right $ String v
str [Primitive (Syntax   name _)] = Right $ String name
str [Primitive (Function name _)] = Right $ String name
str vs =
    String . concat . fStrs <$> mapM (str . (:[])) vs
  where
    fStrs ls = [ s | String s <- ls ]

-- | Bindings of all primitives in the module.
primitives :: Bindings
primitives = Map.fromList
    [ ("eval",        Primitive $ Syntax "eval" eval)
    , ("if",          Primitive $ Syntax "if" if')
    , ("str",         Primitive $ Function "str" str)
    ]
