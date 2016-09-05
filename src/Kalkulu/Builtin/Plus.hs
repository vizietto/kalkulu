{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Plus(plus) where

import Kalkulu.Builtin
import Kalkulu.BuiltinSymbol as B
import qualified Data.Vector as V
import Kalkulu.VectorPattern

plus :: BuiltinCode
plus = defaultBuiltin {
  attributes = [Flat, Listable, NumericFunction, OneIdentity,
                Orderless, Protected],
  downcode   = Just $ return . downcodePlus
  }

-- Extract the "multiplicative constant" before a factor
-- Times[2, a]  ==> (2, a)
-- a            ==> (1, a)
scalarExpr :: Expression -> (Integer, Expression)
scalarExpr (Number x)                      = (x, Number 1)
scalarExpr (CmpB B.Times [Number x, y])    = (x, y)
scalarExpr (CmpB B.Times (Number x :< xs)) = (x, CmpB B.Times xs)
scalarExpr e                               = (1, e)

addArgs :: [Expression] -> [Expression]
addArgs [] = []
addArgs [e] = [e]
addArgs (e1 : e2 : es) = let (x1, e1') = scalarExpr e1
                             (x2, e2') = scalarExpr e2 in
    if e1' == e2'
      then addArgs $ (CmpB B.Times [Number (x1+x2), e1']) : es
      else e1 : (addArgs (e2 : es))
               

downcodePlus :: V.Vector Expression -> Expression
downcodePlus args = case addArgs (V.toList args) of
  []  -> Number 0
  [e] -> e
  es  -> CmpB B.Plus (V.fromList es)
