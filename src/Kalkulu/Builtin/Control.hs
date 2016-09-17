{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Control (controlBuiltins) where

import Control.Monad

import qualified Data.Vector as V
import Kalkulu.Builtin
import qualified Kalkulu.Pattern as Pattern
import qualified Kalkulu.BuiltinSymbol as B

controlBuiltins :: [(B.BuiltinSymbol, BuiltinDefinition)]
controlBuiltins = [
    (B.CompoundExpression, compoundExpression)
  -- , (B.Return, return_)
  -- , (B.Catch, catch)
  -- , (B.Throw, throw)
  -- , (B.Goto, goto)
  -- , (B.Label, label)
  -- , (B.If, if_)
  -- , (B.Switch, switch)
  -- , (B.Which, which)
  -- , (B.Do, do_)
  -- , (B.For, for)
  -- , (B.While, while)
  -- , (B.Nest, nest)
  -- , (B.NestList, nestList)
  -- , (B.NestWhile, nestWhile)
  -- , (B.FixedPoint, fixedPoint)
  -- , (B.FixedPointList, fixedPointList)
  -- , (B.Abort, abort)
  -- , (B.Break, break)
  -- , (B.Continue, continue)
  ]

compoundExpression :: BuiltinDefinition
compoundExpression = defaultBuiltin {
    attributes = [HoldAll, Protected] -- no ReadProtected
  , downcode   = downcodeCompoundExpression
  }

downcodeCompoundExpression :: Expression -> Kernel Expression
downcodeCompoundExpression e@(Cmp _ []) = return e
downcodeCompoundExpression (Cmp _ args) = do
  args_ev <- V.mapM evaluate args
  return $ V.last args_ev

if_ :: BuiltinDefinition
if_ = defaultBuiltin {
  attributes = [HoldRest, Protected],
  downcode   = downcodeIf
  }

downcodeIf :: Expression -> Kernel Expression
downcodeIf e@(Cmp _ args) = do
  when (length args < 2 || length args > 4) (return ()) -- sendMessage
  return $ pureIf e
downcodeIf _ = error "unreachable"

pureIf :: Expression -> Expression
pureIf (Cmp _ [SymbolB B.True, a])        = a
pureIf (Cmp _ [SymbolB B.False, _])       = SymbolB B.Null
pureIf (Cmp _ [SymbolB B.True, a, _])     = a
pureIf (Cmp _ [SymbolB B.False, _, a])    = a
pureIf (Cmp _ [SymbolB B.True, a, _, _])  = a
pureIf (Cmp _ [SymbolB B.False, _, a, _]) = a
pureIf (Cmp _ [_, _, _, a])               = a
pureIf e                                  = e
