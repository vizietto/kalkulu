{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.SameQ(sameQ) where

import Kalkulu.Builtin
import Kalkulu.VectorPattern

sameQ :: BuiltinDefinition
sameQ = defaultBuiltin { downcode = return . toExpression . pureSameQ }

pureSameQ :: Expression -> Bool
pureSameQ (Cmp _ [])       = True
pureSameQ (Cmp _ (h :< t)) = all (== h) t
pureSameQ _                = error "unreachable"
