{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Plus(plus) where

import Kalkulu.Builtin
import qualified Kalkulu.BuiltinSymbol as B
import qualified Data.Vector as V
import Kalkulu.VectorPattern

plus :: BuiltinDefinition
plus = defaultBuiltin {
  attributes = [Flat, Listable, NumericFunction, OneIdentity,
                Orderless, Protected],
  downcode   = return . purePlus
  }

-- Extract the "multiplicative constant" before a factor
-- Times[2, a]  ==> (2, a)
-- a            ==> (1, a)
scalarExpr :: Expression -> (Integer, Expression)
scalarExpr (Number x)                      = (x, Number 1)
scalarExpr (CmpB B.Times [Number x, y])    = (x, y)
scalarExpr (CmpB B.Times (Number x :< xs)) = (x, CmpB B.Times xs)
scalarExpr e                               = (1, e)

-- reciprocal function
unscalarExpr :: (Integer, Expression) -> Expression
unscalarExpr (x, Number 1) = Number x
unscalarExpr (x, CmpB B.Times ys) = CmpB B.Times (V.cons (Number x) ys)
unscalarExpr (x, y) = CmpB B.Times [Number x, y]

addArgs :: [Expression] -> [Expression]
addArgs [] = []
addArgs [e] = [e]
addArgs (e1 : e2 : es) = let (x1, e1') = scalarExpr e1
                             (x2, e2') = scalarExpr e2 in
  if e1' == e2'
    then addArgs $ (unscalarExpr (x1+x2, e1')) : es
    else e1 : (addArgs (e2 : es))
               

purePlus :: Expression -> Expression
purePlus (Cmp _ args) = f $ addArgs $ V.toList args
  where f [] = Number 0
        f [e] = e
        f ((Number 0) : es) = f es
        f es = CmpB B.Plus (V.fromList es)
purePlus _ = error "unreachable"
