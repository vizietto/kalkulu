{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Times(times) where

import Kalkulu.Builtin
import qualified Kalkulu.BuiltinSymbol as B
import qualified Data.Vector as V

times :: BuiltinDefinition
times = defaultBuiltin {
  attributes = [Flat, Listable, NumericFunction, OneIdentity,
                Orderless, Protected],
  downcode   = return . pureTimes
  }

baseExp :: Expression -> (Expression, Expression)
baseExp (CmpB B.Power [a, b]) = (a, b)
baseExp e = (e, Number 1)

unBaseExp :: (Expression, Expression) -> Expression
unBaseExp (e, Number 1) = e
unBaseExp (_, Number 0) = Number 1
unBaseExp (a, b) = CmpB B.Power [a, b]

-- to sum exponents
plus :: Expression -> Expression -> Expression
plus (Number x) (Number y) = Number (x + y)
plus x y = CmpB B.Plus [x, y]

pureTimes :: Expression -> Expression
pureTimes (Cmp _ args) = case timesArgs (V.toList args) of
  []  -> Number 1
  [e] -> e
  (Number 0):_ -> Number 0 -- TODO: 0*Infinity, 0*ComplexInfinity, etc.
  es  -> CmpB B.Times (V.fromList es)
pureTimes _ = error "unreachable"

timesArgs :: [Expression] -> [Expression]
timesArgs [] = []
timesArgs [e] = [e]
timesArgs (Number x : Number y : es) = timesArgs $ Number (x*y) : es
timesArgs (e1:e2:es)
  | a1 == a2  = timesArgs $ (unBaseExp (a1, plus b1 b2)) : es
  | otherwise = e1 : (timesArgs (e2:es))
  where (a1, b1) = baseExp e1
        (a2, b2) = baseExp e2
