module Kalkulu.Builtin.Plus where

import Kalkulu.Kernel
import Kalkulu.Expression
import Data.IORef
import qualified Data.Vector as V

def :: IO Definition
def = Definition
  <$> newIORef [Orderless, Protected]
  <*> return Nothing
  <*> return Nothing
  <*> return Nothing
  <*> return (Just downcodePlus)

downcodePlus :: V.Vector Expression -> Kernel Expression
downcodePlus = undefined
