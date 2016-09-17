{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Comparison (comparisonBuiltins) where

import Kalkulu.Builtin
import qualified Kalkulu.BuiltinSymbol as B
import Kalkulu.VectorPattern

comparisonBuiltins :: [(B.BuiltinSymbol, BuiltinDefinition)]
comparisonBuiltins = [
    (B.SameQ, sameQ)
  , (B.UnsameQ, unsameQ)
  , (B.TrueQ, trueQ)
  -- , (B.ValueQ, valueQ)
  -- , (B.Inequality, inequality)
  -- , (B.Equal, equal)
  -- , (B.Unequal, unequal)
  -- , (B.Less, less)
  -- , (B.LessEqual, lessEqual)
  -- , (B.Greater, greater)
  -- , (B.GreaterEqual, greaterEqual)
  -- , (B.Positive, positive)
  -- , (B.Negative, negative)
  -- , (B.NonNegative, nonNegative)
  -- , (B.NonPositive, nonPositive)
  -- , (B.Max, max_)
  -- , (B.Min, min_)
  ]

sameQ :: BuiltinDefinition
sameQ = defaultBuiltin { downcode = return . toExpression . pureSameQ }

pureSameQ :: Expression -> Bool
pureSameQ (Cmp _ [])       = True
pureSameQ (Cmp _ (h :< t)) = all (== h) t
pureSameQ _                = error "unreachable"

unsameQ :: BuiltinDefinition
unsameQ = defaultBuiltin {
  downcode = return . toExpression . not . pureSameQ
  }

trueQ :: BuiltinDefinition
trueQ = defaultBuiltin {
  downcode = return . toExpression . pureTrueQ -- TODO: 1 arg
  }

pureTrueQ :: Expression -> Bool
pureTrueQ (Cmp _ [SymbolB B.True]) = True
pureTrueQ _ = False
