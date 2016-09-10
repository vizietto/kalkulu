{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.AtomQ(atomQ) where

import Control.Monad (when)
import Kalkulu.Builtin
import Kalkulu.BuiltinSymbol as B

atomQ :: BuiltinDefinition
atomQ = defaultBuiltin {
  downcode   = downcodeAtomQ
  }

downcodeAtomQ :: Expression -> Kernel Expression
downcodeAtomQ e@(Cmp _ args) = do
  when (length args /= 1) undefined -- TODO: sendMessage
  return $ pureAtomQ e
downcodeAtomQ _ = error "unreachable"

pureAtomQ :: Expression -> Expression
pureAtomQ (Cmp _ [Cmp _ _]) = SymbolB B.False
pureAtomQ (Cmp _ [_])       = SymbolB B.True
pureAtomQ e                 = e
