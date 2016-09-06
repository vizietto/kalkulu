{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Attributes(attributes_) where

import Control.Monad (when)
import Kalkulu.Builtin
import qualified Kalkulu.BuiltinSymbol as B

attributes_ :: BuiltinCode
attributes_ = defaultBuiltin {
  attributes = [HoldAll, Listable, Protected],
  downcode   = downcodeAttributes
  }

downcodeAttributes :: Expression -> Kernel Expression
downcodeAttributes (Cmp _ [Symbol s]) = toExpression <$> getAttributes s
downcodeAttributes (Cmp _ [String s]) =
  toExpression <$> (getSymbol s >>= getAttributes)
downcodeAttributes e@(Cmp _ [_]) = do
  -- TODO: sendMessage Attributes::ssle
  return e
downcodeAttributes e@(Cmp _ _) = do
  -- TODO: sendMessage Attributes::argx
  return e
downcodeAttributes _ = error "unreachable"
