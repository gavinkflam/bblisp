module BBLisp.Kernel
    (
      -- * Syntactic Forms
      eval
      -- * Functions
    , render
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
        Right vs -> (,) b <$> f b (List vs)
eval b v@(Boolean _) = Right (b, v)
eval b v@(Integer _) = Right (b, v)
eval b v@(Decimal _) = Right (b, v)
eval b v@(String _)  = Right (b, v)
eval b (Symbol name) =
    case b !? name of
        Nothing -> Left $ "Binding '" ++ name ++ "' not found"
        Just v  -> Right (b, v)
eval _ l = Left $ "Unexpected form: " ++ show l

-- | Render a literal as string.
render :: Function
render _ (List [s@(String _)]) = Right s
render _ (List [Integer i])    = Right $ String $ show i
render _ l                     = Left $ "Unexpected form: " ++ show l

-- | Binding of all primitives in the module.
primitives :: Binding
primitives = Map.fromList
    [ ("eval",    Primitive $ Syntax "eval" eval)
    , ("render",  Primitive $ Function "render" render)
    ]
