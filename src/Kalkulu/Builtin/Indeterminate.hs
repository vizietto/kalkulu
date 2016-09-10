module Kalkulu.Builtin.Indeterminate(indeterminate) where

import Kalkulu.Builtin
import qualified Kalkulu.BuiltinSymbol as B

indeterminate :: BuiltinDefinition
indeterminate = defaultBuiltin {
  upcode = upcodeIndeterminate
  }

upcodeIndeterminate :: Expression -> Kernel Expression
upcodeIndeterminate e@(Cmp (Symbol s) _) = do
  hasNumericFunction <- s `hasAttribute` NumericFunction
  return $ if hasNumericFunction
    then SymbolB B.Indeterminate
    else e
upcodeIndeterminate e = return e
