{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.Function (functionBuiltins) where

import Control.Monad

import qualified Data.Vector as V
import Kalkulu.Builtin
import qualified Kalkulu.Pattern as Pattern
import qualified Kalkulu.BuiltinSymbol as B

functionBuiltins :: [(B.BuiltinSymbol, BuiltinDefinition)]
functionBuiltins = [
    (B.Function, function)
  , (B.Slot, slot)
  , (B.SlotSequence, slotSequence)
  , (B.Composition, composition)
  , (B.Identity, identity)
  , (B.InverseFunction, inverseFunction)
  ]

function :: BuiltinDefinition
function = defaultBuiltin {
    attributes = [HoldAll, Protected]
  , subcode    = subcodeFunction
  }

subcodeFunction :: Expression -> Kernel Expression
subcodeFunction (Cmp (Cmp _ [body]) args) = subst body args
  where
    subst e@(CmpB B.Slot []) args = case args V.!? 0 of
      Nothing -> return e -- TODO: send message
      Just a  -> return a
    subst e@(CmpB B.Slot [Number n]) args =
      case args V.!? (fromIntegral n) of
        Nothing -> return e -- TODO: Send Message
        Just a  -> return a
    subst (CmpB B.SlotSequence []) args = return $ CmpB B.Sequence args
    subst (CmpB B.SlotSequence [Number n]) args =
      return $ CmpB B.Sequence (V.drop (fromIntegral n) args)
    subst (Cmp h as) args = do
      h' <- subst h args
      as' <- V.mapM (flip subst args) as
      return $ Cmp h' as'
    subst e _ = return e

slot :: BuiltinDefinition
slot = defaultBuiltin {
  attributes = [NHoldAll, Protected]
  }

slotSequence :: BuiltinDefinition
slotSequence = defaultBuiltin {
  attributes = [NHoldAll, Protected]
  }

composition :: BuiltinDefinition
composition = defaultBuiltin {
    attributes = [Flat, OneIdentity, Protected]
  , downcode   = return . pureComposition
  , subcode    = subcodeComposition
  }

pureComposition :: Expression -> Expression
pureComposition (Cmp _ args) = case nonTrivialArgs of
  []  -> SymbolB B.Identity
  [f] -> f
  _   -> CmpB B.Composition nonTrivialArgs
  where nonTrivialArgs = V.filter (/= SymbolB B.Identity) args 
pureComposition _ = error "unreachable"

subcodeComposition :: Expression -> Kernel Expression
subcodeComposition (Cmp (Cmp _ fs) args) | not (V.null fs) =
  return $ V.foldr (\f y -> Cmp f [y]) (Cmp (V.last fs) args) (V.init fs)
subcodeComposition e = return e

identity :: BuiltinDefinition
identity = defaultBuiltin {
  downcode = return . pureIdentity -- TODO: 1 arg
  }

pureIdentity :: Expression -> Expression
pureIdentity (Cmp _ [e]) = e
pureIdentity e = e

inverseFunction :: BuiltinDefinition
inverseFunction = defaultBuiltin {
    attributes = [NHoldAll, Protected]
  , upcode     = upcodeInverseFunction
  , subcode    = subcodeInverseFunction
  }

upcodeInverseFunction :: Expression -> Kernel Expression
upcodeInverseFunction (Cmp h [Cmp (CmpB B.InverseFunction [f]) [a]])
  | h == f = return a -- send message
upcodeInverseFunction (Cmp h [Cmp (CmpB B.InverseFunction [f]) args]) =
  return $ CmpB B.Identity args -- send message
upcodeInverseFunction e = return e

subcodeInverseFunction :: Expression -> Kernel Expression
subcodeInverseFunction (Cmp (Cmp _ [e]) [Cmp h [a]])
  | e == h = return a -- send message
subcodeInverseFunction (Cmp (Cmp _ [e]) [Cmp h args])
  | e == h = return $ CmpB B.Identity args -- send message
subcodeInverseFunction e = return e

