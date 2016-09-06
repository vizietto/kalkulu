{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Head(head_) where

import Control.Monad (when)
import Kalkulu.Builtin
import qualified Kalkulu.BuiltinSymbol as B

head_ :: BuiltinCode
head_  = defaultBuiltin {
  downcode   = downcodeHead
  }

downcodeHead :: Expression -> Kernel Expression
downcodeHead e@(Cmp _ args) = do
  when (length args /= 1) undefined -- TODO: sendMessage
  return $ pureHead e
downcodeHead _ = error "unreachable"

pureHead :: Expression -> Expression
pureHead (Cmp _ [Number _]) = SymbolB B.Integer
pureHead (Cmp _ [String _]) = SymbolB B.String
pureHead (Cmp _ [Symbol _]) = SymbolB B.Symbol
pureHead (Cmp _ [Cmp h _])  = h
pureHead e                  = e
