{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Evaluation where

import qualified Data.Vector as V

import Data.Array
import Data.IORef
import Data.List (sort)
import Data.Maybe (isJust, fromJust)
import qualified Data.Vector.Mutable as MV
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Kalkulu.VectorPattern

import qualified Kalkulu.Builtin as B
import Kalkulu.Expression
import Kalkulu.Symbol

type Kernel = ReaderT Environment IO

data Attribute = Constant
               | Flat
               | HoldAll
               | HoldAllComplete
               | HoldFirst
               | HoldRest
               | Listable
               | Locked
               | NHoldAll
               | NHoldRest
               | NumericFunction
               | OneIdentity
               | Orderless
               | Protected
               | SequenceHold
               | Stub
               | Temporary
               deriving Eq

data Definition = Definition {
    name       :: String
  , context    :: String
  , attributes :: IORef [Attribute]
  , ownvalue   :: Maybe Expression
  , owncode    :: Maybe (Kernel Expression)
  , upcode     :: Maybe (Expression -> Kernel Expression)
  , subcode    :: Maybe (Expression -> Kernel Expression)
  , downcode   :: Maybe (V.Vector Expression -> Kernel Expression)
  }

data Environment = Environment {
    moduleNumber   :: IORef Int
  , currentContext :: IORef String
  , builtinDefs    :: Array B.BuiltinSymbol Definition
  , defs           :: MV.IOVector Definition
  }

getDef :: Symbol -> Kernel Definition
getDef symb = do
  env <- ask
  case symb of
    Builtin s        -> return $ (builtinDefs env) ! s
    UserSymbol i _ _ -> lift $ MV.read (defs env) i

hasAttribute :: Symbol -> Attribute -> Kernel Bool
x `hasAttribute` att = do
  def <- getDef x
  atts <- lift $ readIORef (attributes def)
  return $ att `elem` atts

next :: Expression -> Kernel Expression -> Kernel Expression
next before after = after >>= reEval
  where reEval e = if e == before then return e else eval e

eval :: Expression -> Kernel Expression
eval (Cmp hd@(Cmp _ _) args) = do
  hd' <- eval hd
  e' <- (Cmp hd') <$> (V.mapM eval args)
  if hd /= hd'
    then eval e'
    else next e' $ (applySubValue ===> applySubcode) e'
eval (Cmp hd@(Symbol x) args) = do
  hd' <- eval hd
  if hd /= hd'
    then eval (Cmp hd' args)
    else do e' <- (Cmp hd') <$> (evalArgs x args)
            next e' $ applyRules e'

eval (Cmp hd args) = Cmp <$> (eval hd) <*> (V.mapM eval args)

eval e@(Symbol _) = next e $ (applyOwnValue ===> applyOwncode) e

eval e = return e

evalArgs :: Symbol -> V.Vector Expression -> Kernel (V.Vector Expression)
evalArgs x args = do
  hasHoldAll   <- x `hasAttribute` HoldAll
  hasHoldFirst <- x `hasAttribute` HoldFirst
  hasHoldRest  <- x `hasAttribute` HoldRest
  case (hasHoldAll, hasHoldFirst, hasHoldRest) of
    (True, _, _) -> return args
    (_, True, _) -> evalFirst args
    (_, _, True) -> evalRest  args
    _            -> evalAll   args
  where
    evalFirst [] = return V.empty
    evalFirst es = V.cons <$> (eval $ V.head es)
                          <*> (pure $ V.tail es)
    evalRest []  = return V.empty
    evalRest es  = V.cons <$> (pure $ V.head es)
                          <*> (V.mapM eval $ V.tail es)
    evalAll      = V.mapM eval

-- flattens
flattenArgs :: Symbol -> V.Vector Expression -> Kernel (V.Vector Expression)
flattenArgs x args = do
  hs <- forbiddenHeads
  return $ V.concatMap (flatten hs) args
  where forbiddenHeads = do
          hasHoldAllComplete <- x `hasAttribute` HoldAllComplete
          hasSequenceHold    <- x `hasAttribute` SequenceHold
          hasFlat            <- x `hasAttribute` Flat
          return $ case (hasHoldAllComplete || hasSequenceHold, hasFlat) of
            (True, True)   -> [x]
            (True, False)  -> []
            (False, True)  -> [Builtin B.Sequence, x]
            (False, False) -> [Builtin B.Sequence]
        flatten :: [Symbol] -> Expression -> V.Vector Expression
        flatten forbidden (Cmp (Symbol s) es) | s `elem` forbidden = es
        flatten _ e = V.singleton e

-- See if head can be threaded over lists
threadLists :: Symbol -> V.Vector Expression -> Kernel (V.Vector Expression)
threadLists x args = do
  hasListable <- x `hasAttribute` Listable
  if hasListable
    then do
      let lengths = V.filter isJust (V.map length' args)
      case lengths of
        []                    -> return args
        h :< t | all (== h) t -> let l = fromJust h in
              return $ V.map applyHead $ tranpose $ V.map (listify l) args
               | otherwise    -> return args -- TODO sendWarning
    else return args
  where
    length' :: Expression -> Maybe Int
    length' (Cmp (SymbolB B.List) es)   = Just (V.length es)
    length' _                           = Nothing
    listify _ (Cmp (SymbolB B.List) es) = es
    listify n e                         = V.replicate n e
    applyHead = Cmp (Symbol x)
    tranpose = undefined

sortArgs :: Symbol -> V.Vector Expression -> Kernel (V.Vector Expression)
sortArgs x args = do
  hasOrderless <- x `hasAttribute` Orderless
  if hasOrderless
    then return $ sortVector args
    else return args
  where sortVector = V.fromList . sort . V.toList

processArgs :: Symbol -> V.Vector Expression -> Kernel (V.Vector Expression)
processArgs x =
  (evalArgs x) >=> (flattenArgs x) >=> (threadLists x) >=> (sortArgs x)


-- apply builtin code, first try upcode, then downcode
applyBuiltinRules :: Expression -> Kernel Expression
applyBuiltinRules = applyUpcode ===> applyDowncode

(===>) :: (Expression -> Kernel Expression) -> (Expression -> Kernel Expression) -> (Expression -> Kernel Expression)
(f ===> g) e = do
  e' <- f e
  if e == e' then g e else return e'

applyUserRules :: Expression -> Kernel Expression
applyUserRules = return . id -- TODO: modify

applyRules :: Expression -> Kernel Expression
applyRules = applyUserRules ===> applyBuiltinRules

applyDowncode :: Expression -> Kernel Expression
applyDowncode e@(Cmp (Symbol x) args) = do
  code <- getDowncode x
  case code of
    Nothing -> return e
    Just f  -> next e $ f args
  where getDowncode symb = getDef symb >>= return . downcode
applyDowncode _ = error "unreachable"

applyUpcode :: Expression -> Kernel Expression
applyUpcode e@(Cmp _ []) = return e
applyUpcode e@(Cmp _ args) = do
  upcodes <- V.mapM (getUpcode . superHead) args
  foldl1 (===>) upcodes $ e
  where getUpcode (Symbol s) = do
          def <- getDef s
          return $ case (upcode def) of
            Nothing -> return . id
            Just f  -> f
        getUpcode _ = return (return . id)
applyUpcode _ = error "unreachable"

applySubValue :: Expression -> Kernel Expression
applySubValue = return . id -- TODO

applySubcode :: Expression -> Kernel Expression
applySubcode e = case (superHead e) of
  Symbol s -> do def <- getDef s
                 case (subcode def) of
                   Nothing -> return e
                   Just f  -> f e
  _        -> return e

applyOwnValue :: Expression -> Kernel Expression
applyOwnValue = return . id -- TODO

applyOwncode :: Expression -> Kernel Expression
applyOwncode e@(Symbol s) = do
  def <- getDef s
  case (owncode def) of
    Nothing -> return e
    Just f  -> f
applyOwncode _ = error "unreachable"

sendWarning :: a
sendWarning = undefined
