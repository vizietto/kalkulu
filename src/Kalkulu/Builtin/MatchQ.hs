{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.MatchQ(matchQ) where

import Control.Monad

import Kalkulu.Builtin
import Kalkulu.Pattern
import qualified Kalkulu.BuiltinSymbol as B

matchQ :: BuiltinDefinition
matchQ = defaultBuiltin {
  downcode   = downcodeMatchQ
  }

downcodeMatchQ :: Expression -> Kernel Expression
downcodeMatchQ (Cmp _ [e, p]) =
  matchPattern e (toPattern p) >>= return . toExpression . not . null
downcodeMatchQ e = return e -- TODO send Message
