{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Length(length_) where

import Control.Monad
import Kalkulu.Builtin

length_ :: BuiltinDefinition
length_ = defaultBuiltin {
  downcode   = downcodeLength
  }

downcodeLength :: Expression -> Kernel Expression
downcodeLength e@(Cmp _ args) = do
  when (length args /= 1) undefined -- TODO: sendMessage
  return $ pureLength e

pureLength :: Expression -> Expression
pureLength (Cmp _ [Cmp _ args]) = toExpression (length args)
pureLength (Cmp _ [_])          = toExpression (0 :: Integer)
pureLength e                    = e
