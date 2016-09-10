module Kalkulu.Builtin(module Kalkulu.Expression,
                       module Kalkulu.Kernel,
                       Attribute(..),
                       BuiltinDefinition(..),
                       defaultBuiltin,
                       evaluate
                       ) where

import Kalkulu.Kernel hiding (owncode, upcode, subcode, downcode)
import Kalkulu.Expression
import Kalkulu.Evaluation (evaluate)
-- import qualified Kalkulu.BuiltinSymbol as B

data BuiltinDefinition = BuiltinDefinition {
  attributes :: [Attribute],
  owncode    :: Maybe (Kernel Expression),
  upcode     :: Expression -> Kernel Expression,
  subcode    :: Expression -> Kernel Expression,
  downcode   :: Expression -> Kernel Expression
  }

defaultBuiltin :: BuiltinDefinition
defaultBuiltin = BuiltinDefinition {
  attributes = [Protected],
  owncode    = Nothing,
  upcode     = return . id,
  subcode    = return . id,
  downcode   = return . id
  }
