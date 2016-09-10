{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Head(head_) where

import Control.Monad (when)
import Kalkulu.Builtin
import qualified Kalkulu.BuiltinSymbol as B

head_ :: BuiltinDefinition
head_  = defaultBuiltin { downcode   = downcodeHead }

downcodeHead :: Expression -> Kernel Expression
downcodeHead e@(Cmp _ args) = do
  when (length args /= 1) undefined -- TODO: sendMessage
  return $ getHead e
downcodeHead _ = error "unreachable"
