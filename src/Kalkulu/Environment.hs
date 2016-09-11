module Kalkulu.Environment (defaultEnvironment,
                            Environment(..),
                            Definition(..),
                            emptyDefinition,
                            run) where

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Trans.Writer
import Data.IORef
import qualified Data.Map as Map
import Data.Array
import Kalkulu.Symbol
import Kalkulu.BuiltinSymbol as B
import qualified Kalkulu.Builtin as BD

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

data Environment = Environment {
    symbolNumber   :: IORef Int
  , iterationLimit :: IORef (Maybe Int)
  , recursionLimit :: IORef (Maybe Int)
  , context        :: IORef String
  , contextPath    :: IORef [String]
  , symbolTable    :: IORef (Map.Map (ContextName, SymbolName) Symbol)
  , builtinDefs    :: Array B.BuiltinSymbol Definition
  , defs           :: IORef (Map.Map SymbolId Definition)
  }

data Definition = Definition {
    attributes  :: IORef [Attribute]
  , builtinCode :: BuiltinCode
  }

emptyDefinition :: IO Definition
emptyDefinition = Definition
  <$> newIORef []
  <*> return (BuiltinCode Nothing (return . id) (return . id) (return . id))

toDefinition :: BD.BuiltinDefinition -> IO Definition
toDefinition code = Definition
  <$> newIORef (BD.attributes code)
  <*> return (BuiltinCode (BD.owncode code) (BD.upcode code)
                          (BD.subcode code) (BD.downcode code))

defaultEnvironment :: IO Environment
defaultEnvironment = Environment
  <$> newIORef 0                      -- symbolNumber
  <*> newIORef (Just 4096)            -- iteration limit
  <*> newIORef (Just 1024)            -- recursion limit
  <*> newIORef "Global`"              -- current context
  <*> newIORef ["System`", "Global`"] -- context path
  <*> newIORef (Map.fromList [(("System`", show b), Builtin b) -- symbolTable
                                            | b <- [minBound..]])
  <*> (array (minBound, maxBound) <$> builtin)
  <*> newIORef Map.empty
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
        def _    = emptyDefinition

run :: Environment -> Kernel a -> IO a
run env action =
  let proceed = run env . lift in
  case runIdentity $ runFreeT $ fmap fst $ runWriterT action of
  Pure x -> return x
  Free (GetSymbolMaybe c s next) -> do
    table <- readIORef (symbolTable env)
    proceed $ next $ Map.lookup (c, s) table
  Free (CreateSymbol c s next) -> do
    identNumber <- readIORef (symbolNumber env)
    def <- emptyDefinition
    let symb = UserSymbol identNumber s c
    modifyIORef (defs env) (Map.insert identNumber def)
    modifyIORef (symbolTable env) (Map.insert (c, s) symb)
    modifyIORef (symbolNumber env) (+ 1)
    proceed $ next symb
  Free (GetBuiltinCode (Builtin b) next) ->
    proceed $ next $ builtinCode (builtinDefs env ! b)
  Free (GetBuiltinCode _ next) ->
    let emptyBuiltinCode = BuiltinCode {
            owncode  = Nothing
          , upcode   = return . id
          , downcode = return . id
          , subcode  = return . id
          } in proceed $ next $ emptyBuiltinCode
  Free (HasAttribute s at next) -> do
    def <- getDef env s
    attrs <- readIORef (attributes def)
    proceed $ next $ at `elem` attrs
  Free (GetIterationLimit next) ->
    readIORef (iterationLimit env) >>= proceed . next
  Free (SetIterationLimit lim next) -> case lim of
    Just l | l < 20 -> error "Attempt to set $IterationLimit under 20"
    _ -> writeIORef (iterationLimit env) lim >> proceed next
  Free (GetRecursionLimit next) ->
    readIORef (recursionLimit env) >>= proceed . next
  Free (SetRecursionLimit lim next) -> case lim of
    Just l | l < 20 -> error "Attempt to set $RecursionLimit under 20"
    _ -> writeIORef (recursionLimit env) lim >> proceed next
  Free (GetCurrentContext next) ->
    readIORef (context env) >>= proceed . next
  Free (GetContextPath next) ->
    readIORef (contextPath env) >>= proceed . next
  -- _ -> undefined

getDef :: Environment -> Symbol -> IO Definition
getDef env (Builtin b) = return $ (builtinDefs env) ! b
getDef env (UserSymbol i _ _) = do
  definitions <- readIORef (defs env)
  maybe emptyDefinition return (Map.lookup i definitions)