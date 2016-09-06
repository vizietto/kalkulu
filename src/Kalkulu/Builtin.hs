module Kalkulu.Builtin(module Kalkulu.Expression,
                       module Kalkulu.Kernel,
                       Attribute(..),
                       BuiltinCode(..),
                       defaultBuiltin,
                       toDefinition
                       ) where

import Data.IORef
import Kalkulu.Kernel hiding (attributes, owncode, upcode,
                              subcode, downcode)
import qualified Kalkulu.Kernel as K
import Kalkulu.Expression
-- import qualified Kalkulu.BuiltinSymbol as B

data BuiltinCode = BuiltinCode {
  attributes :: [Attribute],
  owncode    :: Maybe (Kernel Expression),
  upcode     :: Expression -> Kernel Expression,
  subcode    :: Expression -> Kernel Expression,
  downcode   :: Expression -> Kernel Expression
  }

defaultBuiltin :: BuiltinCode
defaultBuiltin = BuiltinCode {
  attributes = [Protected],
  owncode    = Nothing,
  upcode     = return . id,
  subcode    = return . id,
  downcode   = return . id
  }

toDefinition :: BuiltinCode -> IO K.Definition
toDefinition code = K.Definition
  <$> newIORef (attributes code)
  <*> return (owncode code)
  <*> return (upcode code)
  <*> return (subcode code)
  <*> return (downcode code)
