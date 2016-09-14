{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Flatten(flatten) where

import Control.Monad

import Kalkulu.Builtin
import qualified Data.Vector as V

flatten :: BuiltinDefinition
flatten = defaultBuiltin {
  downcode   = downcodeFlatten
  }

downcodeFlatten :: Expression -> Kernel Expression
downcodeFlatten (Cmp _ [Cmp h args]) = return $ Cmp h (flattenHead h args)
downcodeFlatten e@(Cmp _ [_]) = return e -- sendMessage
downcodeFlatten e@(Cmp _ args) = do
  when (length args /= 1) undefined -- sendMessage
  return e
downcodeFlatten e = return e

flattenHead :: Expression -> V.Vector Expression -> V.Vector Expression
flattenHead h = V.concatMap (help h)
  where help h e@(Cmp h' as)
          | h == h'   = flattenHead h as
        help h e = V.singleton e
