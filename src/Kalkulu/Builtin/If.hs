{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.If(if_) where

import Kalkulu.Builtin
import Kalkulu.BuiltinSymbol as B
import qualified Data.Vector as V

if_ :: BuiltinCode
if_ = defaultBuiltin {
  attributes = [HoldRest, Protected],
  downcode   = Just downcodeIf
  }

downcodeIf :: V.Vector Expression -> Kernel Expression
downcodeIf [SymbolB B.True, a]        = return a
downcodeIf [SymbolB B.False, _]       = return $ SymbolB B.Null
downcodeIf es@[_, _]                  = return $ CmpB B.If es
downcodeIf [SymbolB B.True, a, _]     = return a
downcodeIf [SymbolB B.False, _, a]    = return a
downcodeIf es@[_, _, _]               = return $ CmpB B.If es
downcodeIf [SymbolB B.True, a, _, _]  = return a
downcodeIf [SymbolB B.False, _, a, _] = return a
downcodeIf [_, _, _, a]               = return a
downcodeIf es                         = return $ CmpB B.If es -- sendMessage
