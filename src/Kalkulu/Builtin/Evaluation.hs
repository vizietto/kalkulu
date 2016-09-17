{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Evaluation (evaluationBuiltins) where

import Control.Monad

import qualified Data.Vector as V
import Kalkulu.Builtin
import qualified Kalkulu.Pattern as Pattern
import qualified Kalkulu.BuiltinSymbol as B

evaluationBuiltins :: [(B.BuiltinSymbol, BuiltinDefinition)]
evaluationBuiltins = [
    (B.Hold, hold)
  , (B.HoldComplete, holdComplete)
  , (B.HoldForm, holdForm)
  , (B.Evaluate, evaluate_)
  , (B.Unevaluated, unevaluated)
  , (B.ReleaseHold, releaseHold)
  , (B.Sequence, sequence__)
  ]

hold :: BuiltinDefinition
hold = defaultBuiltin { attributes = [HoldAll, Protected] }

holdComplete :: BuiltinDefinition
holdComplete = defaultBuiltin {
  attributes = [HoldAllComplete, Protected]
  }

holdForm :: BuiltinDefinition
holdForm = defaultBuiltin {
  attributes = [HoldAll, Protected]
  }

evaluate_ :: BuiltinDefinition
evaluate_ = defaultBuiltin {
  downcode   = return . pureEvaluate
  }

pureEvaluate :: Expression -> Expression
pureEvaluate (Cmp _ [e]) = e
pureEvaluate (Cmp _ args) = CmpB B.Sequence args
pureEvaluate _ = error "unreachable"

unevaluated :: BuiltinDefinition
unevaluated = defaultBuiltin {
  attributes = [HoldAllComplete, Protected]
  }

releaseHold :: BuiltinDefinition
releaseHold = defaultBuiltin {
  downcode = return . pureReleaseHold -- TODO: one arg
  }

pureReleaseHold :: Expression -> Expression
pureReleaseHold (Cmp _ [CmpB h [e]])
  | h == B.Hold || h == B.HoldForm ||
    h == B.HoldPattern || h == B.HoldComplete = e
pureReleaseHold (Cmp _ [CmpB h args])
  | h == B.Hold || h == B.HoldForm ||
    h == B.HoldPattern || h == B.HoldComplete = CmpB B.Sequence args
pureReleaseHold e = e

sequence__ :: BuiltinDefinition
sequence__ = defaultBuiltin
