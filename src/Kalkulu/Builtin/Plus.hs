{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Plus(plus) where

import Kalkulu.Builtin
import Kalkulu.BuiltinSymbol as B
import qualified Data.Vector as V

plus :: BuiltinCode
plus = defaultBuiltin {
  attributes = [Flat, Listable, NumericFunction, OneIdentity,
                Orderless, Protected]
  }
