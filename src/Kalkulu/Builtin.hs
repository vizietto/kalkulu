module Kalkulu.Builtin(module Kalkulu.Expression,
                       Kernel,
                       Attribute(..),
                       BuiltinCode(..),
                       defaultBuiltin,
                       toDefinition
                       ) where

import Data.IORef
import Kalkulu.Kernel (Kernel, Attribute(..))
import qualified Kalkulu.Kernel as K
import Kalkulu.Expression
-- import qualified Kalkulu.BuiltinSymbol as B
import qualified Data.Vector as V

data BuiltinCode = BuiltinCode {
  attributes :: [Attribute],
  owncode    :: Maybe (Kernel Expression),
  upcode     :: Maybe (Expression -> Kernel Expression),
  subcode    :: Maybe (Expression -> Kernel Expression),
  downcode   :: Maybe (V.Vector Expression -> Kernel Expression)
  }

defaultBuiltin :: BuiltinCode
defaultBuiltin = BuiltinCode {
  attributes = [],
  owncode    = Nothing,
  upcode     = Nothing,
  subcode    = Nothing,
  downcode   = Nothing
  }

toDefinition :: BuiltinCode -> IO K.Definition
toDefinition code = K.Definition
  <$> newIORef (attributes code)
  <*> return (owncode code)
  <*> return (upcode code)
  <*> return (subcode code)
  <*> return (downcode code)
