module Kalkulu.Evaluation where

import qualified Data.Vector as V

import Control.Monad

import Kalkulu.Expression
import Kalkulu.Symbol

type Kernel = Expression

next :: Expression -> Kernel Expression -> Kernel Expression
next before after = after >>= reEval
  where reEval e = if e == before then return e else eval e

eval :: Expression -> Kernel Expression
eval e@(Cmp hd@(Cmp _ _) args) = do
  hd' <- eval hd
  let e' = liftM (Cmp hd') (V.mapM eval args)
  case hd' of
    Cmp (Symbol x) _ -> undefined -- next e $ x#subcode <*> e'
    Cmp _ _ -> e'
    _       -> eval (Cmp hd' args)
eval e@(Cmp hd@(Symbol x) args) = do
  hd' <- eval hd
  if hd != hd'
    then eval (Cmp hd' args)
    else undefined
eval (Cmp hd args) = liftM2 Cmp (eval hd) (V.mapM eval args)
eval e@(Symbol x) = undefined
eval e            = return e

evalArgs1 :: Symbol -> Vector Expression -> Kernel Expression
evalArgs1 x args = if x `hasAttributes` HoldAll
  then args
  else if x `hasAttributes` HoldFirst
  then evalFirst args
  else if x `hasAttributes` HoldRest
  then evalRest args
  else V.mapM eval args
  where evalFirst [] = V.empty
        evalFirst (a:args) = V.cons <$> (eval a) (pure args)
        evalRest [] = V.empty
        evalRest (a:args) = V.cons <$> (pure a) (V.mapM eval args)

hasAttributes :: Symbol -> Attribute -> Bool
hasAttributes = undefined

data Attribute = HoldAll | HoldFirst | HoldRest
