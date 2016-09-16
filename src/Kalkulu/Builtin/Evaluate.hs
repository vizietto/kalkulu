{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Evaluate(evaluate_) where

import Kalkulu.Builtin
import qualified Kalkulu.BuiltinSymbol as B

evaluate_ :: BuiltinDefinition
evaluate_ = defaultBuiltin {
  downcode   = return . pureEvaluate
  }

pureEvaluate :: Expression -> Expression
pureEvaluate (Cmp _ [e]) = e
pureEvaluate (Cmp _ args) = CmpB B.Sequence args
pureEvaluate _ = error "unreachable"
