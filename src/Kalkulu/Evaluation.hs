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
  , subcode    :: Maybe (V.Vector Expression -> V.Vector Expression
                                             -> Kernel Expression)
  ,  downcode   :: Maybe (V.Vector Expression -> Kernel Expression)
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
eval e@(Cmp hd@(Cmp _ _) args) = do
  hd' <- eval hd
  e' <- (Cmp hd') <$> (V.mapM eval args)
  case hd' of
    Cmp (Symbol x) args' -> do
      def <- getDef x
      case (subcode def) of
        Nothing   -> return e'
        Just code -> next e' $ code args' args
    Cmp _ _ -> return e'
    _       -> eval (Cmp hd' args)

eval e@(Cmp hd@(Symbol x) args) = do
  hd' <- eval hd
  if hd /= hd'
    then eval (Cmp hd' args)
    else undefined

eval (Cmp hd args) = Cmp <$> (eval hd) <*> (V.mapM eval args)

eval e@(Symbol x) = do
  def <- getDef x
  case (owncode def) of
    Nothing   -> return e
    Just code -> next e $ code

eval e = return e

-- evaluates arguments
evalArgs1 :: Symbol -> V.Vector Expression -> Kernel (V.Vector Expression)
evalArgs1 x args = do
  hasHoldAll   <- x `hasAttribute` HoldAll
  hasHoldFirst <- x `hasAttribute` HoldFirst
  hasHoldRest  <- x `hasAttribute` HoldRest
  case (hasHoldAll, hasHoldFirst, hasHoldRest) of
    (True, _, _) -> return args
    (_, True, _) -> evalFirst args
    (_, _, True) -> evalRest  args
  where
    evalFirst [] = return V.empty
    evalFirst es = V.cons <$> (eval $ V.head es)
                          <*> (pure $ V.tail es)
    evalRest []  = return V.empty
    evalRest es  = V.cons <$> (pure $ V.head es)
                          <*> (V.mapM eval $ V.tail es)

-- flattens
evalArgs2 :: Symbol -> V.Vector Expression -> Kernel (V.Vector Expression)
evalArgs2 x args = do
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
        flatten forbidden e@(Cmp (Symbol x) es)
          | x `elem` forbidden = es
          | otherwise          = V.singleton e

-- See if head can be threaded over lists
evalArgs3 :: Symbol -> V.Vector Expression -> Kernel (V.Vector Expression)
evalArgs3 x args = do
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

evalArgs4 :: Symbol -> V.Vector Expression -> Kernel (V.Vector Expression)
evalArgs4 x args = do
  hasOrderless <- x `hasAttribute` Orderless
  if hasOrderless
    then return $ sortVector args
    else return args
  where sortVector = V.fromList . sort . V.toList

evalArgs :: Symbol -> V.Vector Expression -> Kernel (V.Vector Expression)
evalArgs x =
  (evalArgs1 x) >=> (evalArgs2 x) >=> (evalArgs3 x) >=> (evalArgs4 x)

sendWarning :: a
sendWarning = undefined
