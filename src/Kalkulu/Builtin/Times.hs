{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Times(times) where

import Kalkulu.Builtin
import Kalkulu.BuiltinSymbol as B
import qualified Data.Vector as V
import Kalkulu.VectorPattern

times :: BuiltinCode
times = defaultBuiltin {
  attributes = [Flat, Listable, NumericFunction, OneIdentity,
                Orderless, Protected],
  downcode   = Just $ return . downcodeTimes
  }

baseExp :: Expression -> (Expression, Expression)
baseExp (CmpB B.Power [a, b]) = (a, b)
baseExp e = (e, Number 1)

unBaseExp :: (Expression, Expression) -> Expression
unBaseExp (e, Number 1) = e
unBaseExp (a, b) = CmpB B.Power [a, b]

-- to sum exponents
plus :: Expression -> Expression -> Expression
plus (Number x) (Number y) = Number (x + y)
plus x y = CmpB B.Plus [x, y]

downcodeTimes :: V.Vector Expression -> Expression
downcodeTimes args = case timesArgs (V.toList args) of
  []  -> Number 1
  [e] -> e
  (Number 0):es -> Number 0
  es  -> CmpB B.Times (V.fromList es)

timesArgs :: [Expression] -> [Expression]
timesArgs [] = []
timesArgs [e] = [e]
timesArgs (e1:e2:es)
  | a1 == a2  = timesArgs $ (unBaseExp (a1, plus b1 b2)) : es
  | otherwise = e1 : (timesArgs (e2:es))
  where (a1, b1) = baseExp e1
        (a2, b2) = baseExp e2
