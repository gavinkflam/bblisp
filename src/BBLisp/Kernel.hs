{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module BBLisp.Kernel
    (
      -- * Syntactic Forms
      eval
    , if'
      -- * Functions
    , eq
    , add
    , subtract'
    , str
    , get
    , getIn
      -- * Bindings
    , bindings
    ) where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as Bsc
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import BBLisp.SyntaxTree
    (BBindings, BFunction, BList(..), BPrimitive(..), BSyntax)

-- | Pattern synonym for empty vectors.
pattern Empty :: Vector a
pattern Empty <- (Vector.null -> True)

-- | Split a vector into it's head and tail if applicable.
uncons :: Vector a -> Maybe (a, Vector a)
uncons Empty = Nothing
uncons v     = Just (Vector.unsafeHead v, Vector.unsafeTail v)

-- | Pattern synonym for vector uncons.
infixr 5 :<|
pattern (:<|) :: a -> Vector a -> Vector a
pattern x :<| xs <- (uncons -> Just (x, xs))

-- | Evaluate an expression or definition.
eval :: BSyntax
eval b [v@BNil]         = Right (b, v)
eval b [v@(BBoolean _)] = Right (b, v)
eval b [v@(BInteger _)] = Right (b, v)
eval b [v@(BDecimal _)] = Right (b, v)
eval b [v@(BString _)]  = Right (b, v)
eval b [v@(BDict _)]    = Right (b, v)
eval b [v@(BVector _)]  = Right (b, v)
eval b [BSymbol name]   =
    case b Map.!? name of
        Nothing -> Left $ "Binding for '" ++ Bsc.unpack name ++ "' not found"
        Just v  -> Right (b, v)
eval b [BList [BDict map', BString key]]  =
    case map' Map.!? key of
        Nothing -> Right (b, BNil)
        Just v  -> Right (b, v)
eval _ [BList [BDict _, _]] = Left "Incorrect type for key"
eval b [BList [BVector vector, BInteger index]]  =
    case vector Vector.!? fromIntegral index of
        Nothing -> Right (b, BNil)
        Just v  -> Right (b, v)
eval _ [BList [BVector _, _]] = Left "Incorrect type for index"
eval b [l@(BList (s@(BSymbol _) : ls))] =
    case eval b [s] of
        Left err                    -> Left err
        Right (_, p@(BPrimitive _)) -> eval b [BList (p : ls)]
        Right _                     -> Left $ "Unexpected form: " ++ show l
eval b [BList (BPrimitive (BSyntax _ f) : ls)]   = f b ls
eval b [BList (BPrimitive (BFunction _ f) : ls)] =
    case map snd <$> mapM (eval b . (:[])) ls of
        Left err -> Left err
        Right vs -> (,) b <$> f vs
eval _ l = Left $ "Unexpected form " ++ show l

-- | Evaluates `test`.
--
--   If it produces `true`, evaluate `then` and returns the result.
--   If it produces `false`, evaluate `else` and returns the result, or returns
--   `nil` when there are no `else`.
if' :: BSyntax
if' b [test, then', else'] =
    case eval b [test] of
        Left  err                 -> Left err
        Right (_, BBoolean True)  -> eval b [then']
        Right (_, BBoolean False) -> eval b [else']
        Right (_, _)              -> Left "Incorrect type for `test`."
if' b [test, then'] = if' b [test, then', BNil]
if' _ []            = Left "Too few arguments to if"
if' _ [_]           = Left "Too few arguments to if"
if' _ (_:_:_:_)     = Left "Too many arguments to if"

-- | Returns true if the arguments are of the same type and the values are
--   equivalent.
--
--   Returns false if otherwise.
eq :: BFunction
eq []  = Left "Too few arguments to ="
eq [_] = Left "Too few arguments to ="
eq (headValue : others) = Right $ BBoolean $ all (== headValue) others

-- | Returns the sum of the numbers. (+) returns 0.
add :: BFunction
add [] = Right $ BInteger 0
add arguments
    | all isNumeric arguments = Right $ foldr addNumeric (BInteger 0) arguments
    | otherwise = Left "Arguments should be integers or decimals"
  where
    -- Check if the value is of numeric types.
    isNumeric (BInteger _) = True
    isNumeric (BDecimal _) = True
    isNumeric _ = False
    -- Add two numeric value. Return integer if both values are integer.
    addNumeric (BInteger l) (BInteger r) = BInteger $ l + r
    addNumeric (BInteger l) (BDecimal r) = BDecimal $ fromIntegral l + r
    addNumeric (BDecimal l) (BInteger r) = BDecimal $ l + fromIntegral r
    addNumeric (BDecimal l) (BDecimal r) = BDecimal $ l + r
    addNumeric _ _ = error "Arguments should be integers or decimals"

-- | Subtracts the numbers from the first number.
--
--   (+) returns 0. (- x) returns the negation of x.
subtract' :: BFunction
subtract' = undefined

-- | With one argument, returns the string representation of `v`.
--
--   With more than one argument, returns the concatenation of the string
--   representations of each element of `vs`.
str :: BFunction
str []               = Right $ BString Bsc.empty
str [BBoolean True]  = Right $ BString "true"
str [BBoolean False] = Right $ BString "false"
str [BInteger v]     = Right $ BString $ Bsc.pack $ show v
str [BDecimal v]     = Right $ BString $ Bsc.pack $ show v
str [s@(BString _)]  = Right s
str [BSymbol v]      = Right $ BString v
str [BNil]           = Right $ BString Bsc.empty
str [BPrimitive (BSyntax   name _)] = Right $ BString name
str [BPrimitive (BFunction name _)] = Right $ BString name
str vs =
    BString . Bs.concat . fStrs <$> mapM (str . (:[])) vs
  where
    fStrs ls = [ s | BString s <- ls ]

-- | Returns the value mapped to the key. Returns nil if key not present.
get :: BFunction
get [BDict dictionary, BString key] =
    case Map.lookup key dictionary of
        Nothing  -> Right BNil
        Just val -> Right val
get [_, _] = Right BNil
get arguments
    | length (take 3 arguments) > 2 = Left "Too many arguments to get"
    | otherwise                     = Left "Too few arguments to get"

-- | Returns the value in a nested dictionary using a sequence of keys.
--
--   Returns nil if key not present.
getIn :: BFunction
getIn [BDict _,              BVector Empty]           = Right BNil
getIn [dictionary@(BDict _), BVector (key :<| Empty)] = get [dictionary, key]
getIn [dictionary@(BDict _), BVector (key :<| tailKeys)]  =
    (\v -> getIn [v, BVector tailKeys]) =<< get [dictionary, key]
getIn [_, BVector _] = Right BNil
getIn [_, _]         = Left "Keys should be vector"
getIn arguments
    | length (take 3 arguments) > 2 = Left "Too many arguments to get-in"
    | otherwise                     = Left "Too few arguments to get-in"

-- | All primitives in the module.
bindings :: BBindings
bindings = Map.fromList
    [ ("eval",        BPrimitive $ BSyntax "eval" eval)
    , ("if",          BPrimitive $ BSyntax "if" if')
    , ("=",           BPrimitive $ BFunction "=" eq)
    , ("+",           BPrimitive $ BFunction "+" add)
    , ("-",           BPrimitive $ BFunction "-" subtract')
    , ("str",         BPrimitive $ BFunction "str" str)
    , ("get",         BPrimitive $ BFunction "get" get)
    , ("get-in",      BPrimitive $ BFunction "get-in" getIn)
    ]
