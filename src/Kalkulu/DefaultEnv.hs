module Kalkulu.DefaultEnv (defaultEnv) where

import Data.IORef
import qualified Data.Map as Map
import Data.Array
import Kalkulu.Symbol
import Kalkulu.BuiltinSymbol as B
import Kalkulu.Builtin (toDefinition)
import qualified Data.Vector.Mutable.Dynamic as MV
import qualified Kalkulu.Builtin.If
import qualified Kalkulu.Builtin.Plus
import Kalkulu.Kernel

defaultEnv :: IO Environment
defaultEnv = Environment
  <$> newIORef (Finite 4096)          -- iteration limit
  <*> newIORef (Finite 1024)          -- recursion limit
  <*> newIORef "Global`"              -- current context
  <*> newIORef ["System`", "Global`"] -- context path
  <*> newIORef (Map.fromList [(("System`", show b), Builtin b) -- symbolTable
                                            | b <- [minBound..]])
  <*> (array (minBound, maxBound) <$> builtin)
  <*> (MV.new 0)
  where builtin :: IO [(B.BuiltinSymbol, Definition)]
        builtin = sequence [(,) b <$> def b | b <- [minBound..]]
        def B.If = toDefinition Kalkulu.Builtin.If.if_
        def B.Plus = toDefinition Kalkulu.Builtin.Plus.plus
        def _    = emptyDef
        
