{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Pattern(matchQ,
                               rule,
                               ruleDelayed,
                               replace) where

import Control.Monad

import Kalkulu.Builtin
import Kalkulu.Pattern (matchPattern, toPattern, toRule)
import qualified Kalkulu.Pattern as Pattern
import qualified Kalkulu.BuiltinSymbol as B

matchQ :: BuiltinDefinition
matchQ = defaultBuiltin {
  downcode   = downcodeMatchQ
  }

downcodeMatchQ :: Expression -> Kernel Expression
downcodeMatchQ (Cmp _ [e, p]) =
  matchPattern e (toPattern p) >>= return . toExpression . not . null
downcodeMatchQ e = return e -- TODO send Message

rule :: BuiltinDefinition
rule = defaultBuiltin {
  attributes = [Protected, SequenceHold]
  }

ruleDelayed :: BuiltinDefinition
ruleDelayed = defaultBuiltin {
  attributes = [HoldRest, Protected, SequenceHold]
  }

replace :: BuiltinDefinition
replace = defaultBuiltin {
  downcode = downcodeReplace
  }

downcodeReplace :: Expression -> Kernel Expression
downcodeReplace exp@(Cmp _ [e, r]) = case toRule r of
  Nothing -> return exp -- sendMessage
  Just r' -> Pattern.replace e r'
