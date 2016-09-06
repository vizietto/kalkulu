{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.If(if_) where

import Control.Monad

import Kalkulu.Builtin
import qualified Kalkulu.BuiltinSymbol as B

if_ :: BuiltinCode
if_ = defaultBuiltin {
  attributes = [HoldRest, Protected],
  downcode   = downcodeIf
  }

downcodeIf :: Expression -> Kernel Expression
downcodeIf e@(Cmp _ args) = do
  when (length args < 2 || length args > 4) (return ()) -- sendMessage
  return $ pureIf e
downcodeIf _ = error "unreachable"

pureIf :: Expression -> Expression
pureIf (Cmp _ [SymbolB B.True, a])        = a
pureIf (Cmp _ [SymbolB B.False, _])       = SymbolB B.Null
pureIf (Cmp _ [SymbolB B.True, a, _])     = a
pureIf (Cmp _ [SymbolB B.False, _, a])    = a
pureIf (Cmp _ [SymbolB B.True, a, _, _])  = a
pureIf (Cmp _ [SymbolB B.False, _, a, _]) = a
pureIf (Cmp _ [_, _, _, a])               = a
pureIf e                                  = e
