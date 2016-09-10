{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Logic(true, false, and_) where

import Kalkulu.Builtin
import qualified Kalkulu.BuiltinSymbol as B
import qualified Data.Vector as V
import Kalkulu.VectorPattern

true :: BuiltinDefinition
true = defaultBuiltin { attributes = [Locked, Protected] }

false :: BuiltinDefinition
false = defaultBuiltin { attributes = [Locked, Protected] }

and_ :: BuiltinDefinition
and_ = defaultBuiltin {
  attributes = [Flat, HoldAll, OneIdentity, Protected],
  downcode   = downcodeAnd
}

andArgs :: [Expression] -> Kernel (Maybe [Expression])
andArgs []    = return (Just [])
andArgs (h:t) = do
  h_ev <- evaluate h
  case h_ev of
    SymbolB B.True  -> andArgs t
    SymbolB B.False -> return Nothing
    _               -> andArgs t >>= return . (put h_ev)
  where put _ Nothing  = Nothing
        put h (Just t) = Just (h:t)

downcodeAnd :: Expression -> Kernel Expression
downcodeAnd (Cmp _ args) = do
  args' <- andArgs (V.toList args)
  return $ case args' of
    Nothing -> toExpression False
    Just [] -> toExpression True
    Just es -> CmpB B.And (V.fromList es)
