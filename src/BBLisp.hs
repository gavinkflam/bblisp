module BBLisp
    (
      -- * Bindings
      builtinBindings
      -- * Template
    , runTemplateWith
    , runTemplate
    ) where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Lbs

import qualified BBLisp.Kernel as Ker
import BBLisp.Parser (runParser)
import BBLisp.SyntaxTree (Bindings, List(..))

-- | All builtin bindings shipped with BBLisp.
builtinBindings :: Bindings
builtinBindings = Ker.bindings

-- | Run a template with custom bindings.
runTemplateWith :: Bindings -> Lbs.ByteString -> Either String Bs.ByteString
runTemplateWith bindings template =
    extractString =<< Ker.eval bindings =<< (: []) <$> runParser template
  where
    extractString (_, String str) = Right str
    extractString (_, v) = Left $ "Expected string, but got `" ++ show v ++ "`."

-- | Run a template with builtin bindings.
runTemplate :: Lbs.ByteString -> Either String Bs.ByteString
runTemplate = runTemplateWith builtinBindings
