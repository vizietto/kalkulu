{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Pattern (patternBuiltins) where

import Control.Monad

import qualified Data.Vector as V
import Kalkulu.Builtin
import Kalkulu.Pattern (matchPattern, toPattern, toRule)
import qualified Kalkulu.Pattern as Pattern
import qualified Kalkulu.BuiltinSymbol as B

patternBuiltins :: [(B.BuiltinSymbol, BuiltinDefinition)]
patternBuiltins = [
    (B.HoldPattern, holdPattern)
  , (B.MatchQ, matchQ)
  , (B.Rule, rule)
  , (B.RuleDelayed, ruleDelayed)
  , (B.Replace, replace)
  -- , (B.ReplaceAll, replaceAll)
  -- , (B.ReplaceRepeated, replaceRepeated)
  , (B.ReplaceList, replaceList)
  , (B.PatternTest, patternTest)
  , (B.Pattern, pattern)
  , (B.Condition, condition)
  -- , (B.OptionsPattern, optionsPattern)
  ]

holdPattern :: BuiltinDefinition
holdPattern = defaultBuiltin {
  attributes = [HoldAll, Protected]
  }

matchQ :: BuiltinDefinition
matchQ = defaultBuiltin {
    downcode = downcodeMatchQ -- TODO: one or two args
  , subcode  = subcodeMatchQ  -- TODO: one arg
  }

downcodeMatchQ :: Expression -> Kernel Expression
downcodeMatchQ (Cmp _ [e, p]) = codeMatchQ e p
downcodeMatchQ e = return e

subcodeMatchQ :: Expression -> Kernel Expression
subcodeMatchQ (Cmp (Cmp _ [p]) [e]) = codeMatchQ e p
subcodeMatchQ e = return e

codeMatchQ :: Expression -> Expression -> Kernel Expression
codeMatchQ e p =
  matchPattern e (toPattern p) >>= return . toExpression . not . null

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
    downcode = downcodeReplace -- TODO: decorate levelSpec
  , subcode  = subcodeReplace
  }

downcodeReplace :: Expression -> Kernel Expression
downcodeReplace exp@(Cmp _ [e, CmpB B.List rs]) =
  case sequence (map toRule (V.toList rs)) of
    Nothing  -> return exp
    Just rs' -> (foldl1 (>=>) (map Pattern.replace rs')) e
downcodeReplace exp@(Cmp _ [e, r]) = case toRule r of
  Nothing -> return exp
  Just r' -> Pattern.replace r' e
downcodeReplace e = return e

subcodeReplace :: Expression -> Kernel Expression
subcodeReplace (Cmp (Cmp _ [r]) [e]) = return $ CmpB B.Replace [e, r]

replaceList :: BuiltinDefinition
replaceList = defaultBuiltin { downcode = downcodeReplaceList }

downcodeReplaceList :: Expression -> Kernel Expression
downcodeReplaceList exp@(Cmp _ [e, r]) = case toRule r of
  Nothing -> return exp -- TODO sendMessage
  Just r' -> Pattern.replaceList r' e

patternTest :: BuiltinDefinition
patternTest = defaultBuiltin { attributes = [HoldRest, Protected] }

pattern :: BuiltinDefinition
pattern = defaultBuiltin { attributes = [HoldFirst, Protected] }

condition :: BuiltinDefinition
condition = defaultBuiltin { attributes = [HoldAll, Protected]}
