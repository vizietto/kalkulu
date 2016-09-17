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
  , (B.Label, label)
  , (B.If, if_)
  -- , (B.Switch, switch)
  -- , (B.Which, which)
  -- , (B.Do, do_)
  , (B.For, for)
  , (B.While, while)
  , (B.Nest, nest)
  , (B.NestList, nestList)
  -- , (B.NestWhile, nestWhile)
  -- , (B.NestWhileList, nestWhileList)
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
  downcode   = return . pureIf -- TODO: between 2 and 4 args
  }

label :: BuiltinDefinition
label = defaultBuiltin {
  attributes = [HoldFirst, Protected]
  }

pureIf :: Expression -> Expression
pureIf (Cmp _ [SymbolB B.True, a])        = a
pureIf (Cmp _ [SymbolB B.False, _])       = toExpression ()
pureIf (Cmp _ [SymbolB B.True, a, _])     = a
pureIf (Cmp _ [SymbolB B.False, _, a])    = a
pureIf (Cmp _ [SymbolB B.True, a, _, _])  = a
pureIf (Cmp _ [SymbolB B.False, _, a, _]) = a
pureIf (Cmp _ [_, _, _, a])               = a
pureIf _                                  = error "unreachable"

for :: BuiltinDefinition
for = defaultBuiltin {
    attributes = [HoldAll, Protected]
  , downcode   = downcodeFor -- TODO 3 or 4 arguments
  }

downcodeFor :: Expression -> Kernel Expression
downcodeFor (Cmp _ [a, b, c, d]) = codeFor a b c d
downcodeFor (Cmp _ [a, b, c])    = codeFor a b c (toExpression ())
downcodeFor _                    = error "unreachable"

codeFor :: Expression -> Expression -> Expression -> Expression -> Kernel Expression
codeFor start test incr body = evaluate start >> doLoop
  where doLoop = do test' <- evaluate test
                    if test' == toExpression True
                      then evaluate body >> evaluate incr >> doLoop
                      else return $ toExpression ()
while :: BuiltinDefinition
while = defaultBuiltin {
    attributes = [HoldAll, Protected]
  , downcode   = downcodeWhile -- TODO: 1 or 2 args
  }

downcodeWhile :: Expression -> Kernel Expression
downcodeWhile (Cmp _ [a, b]) = codeWhile a b
downcodeWhile (Cmp _ [a])    = codeWhile a (toExpression ())
downcodeWhile _              = error "unreachable"

codeWhile :: Expression -> Expression -> Kernel Expression
codeWhile test body = do test' <- evaluate test
                         if test' == toExpression True
                           then evaluate body >> codeWhile test body
                           else return $ toExpression ()

nest :: BuiltinDefinition
nest = defaultBuiltin {
  downcode = downcodeNest -- TODO: 3 args
  }

downcodeNest :: Expression -> Kernel Expression
downcodeNest (Cmp _ [f, x, Number n]) | n >= 0
  = return $ (iterate (Cmp f . V.singleton) x) !! (fromInteger n)
downcodeNest e = return e

nestList :: BuiltinDefinition
nestList = defaultBuiltin {
  downcode = downcodeNestList -- TODO: 3 args
  }

downcodeNestList :: Expression -> Kernel Expression
downcodeNestList (Cmp _ [f, x, Number n]) | n >= 0
  = return $ toExpression $ take (fromIntegral n)
                                 (iterate (Cmp f . V.singleton) x)
downcodeNestList e = return e
