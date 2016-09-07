module Kalkulu.DefaultEnv (defaultEnv) where

import Data.IORef
import qualified Data.Map as Map
import Data.Array
import Kalkulu.Symbol
import Kalkulu.BuiltinSymbol as B
import Kalkulu.Builtin (toDefinition)
import qualified Data.Vector.Mutable.Dynamic as MV

import qualified Kalkulu.Builtin.AtomQ
import qualified Kalkulu.Builtin.Attributes
import qualified Kalkulu.Builtin.Head
import qualified Kalkulu.Builtin.Indeterminate
import qualified Kalkulu.Builtin.If
import qualified Kalkulu.Builtin.Logic
import qualified Kalkulu.Builtin.Plus
import qualified Kalkulu.Builtin.Times
import qualified Kalkulu.Builtin.SameQ
import qualified Kalkulu.Builtin.Length
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
        def B.And = toDefinition Kalkulu.Builtin.Logic.and_
        def B.AtomQ = toDefinition Kalkulu.Builtin.AtomQ.atomQ
        def B.Attributes = toDefinition Kalkulu.Builtin.Attributes.attributes_
        def B.False = toDefinition Kalkulu.Builtin.Logic.false
        def B.Head = toDefinition Kalkulu.Builtin.Head.head_
        def B.Indeterminate = toDefinition Kalkulu.Builtin.Indeterminate.indeterminate
        def B.If = toDefinition Kalkulu.Builtin.If.if_
        def B.Length = toDefinition Kalkulu.Builtin.Length.length_
        def B.Plus = toDefinition Kalkulu.Builtin.Plus.plus
        def B.Times = toDefinition Kalkulu.Builtin.Times.times
        def B.True = toDefinition Kalkulu.Builtin.Logic.true
        def B.SameQ = toDefinition Kalkulu.Builtin.SameQ.sameQ
        def _    = emptyDef
