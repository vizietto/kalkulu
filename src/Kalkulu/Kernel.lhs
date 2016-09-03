\documentclass[main.tex]{subfiles}

\begin{document}

\chapter{Kernel}

This chapter describes the module \inline{Kalkulu.Kernel}, whose
header is
\begin{code}
{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Kernel where

import qualified Data.Vector as V

import Data.Array
import Data.IORef
import Data.List (intercalate, find)
import Data.List.Split (splitWhen)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Vector.Mutable.Dynamic as MV
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import qualified Kalkulu.Builtin as B
import Kalkulu.Expression
import Kalkulu.Symbol
\end{code}

\begin{code}
type Kernel = ReaderT Environment IO

data Attribute = Constant | Flat | HoldAll | HoldAllComplete | HoldFirst
  | HoldRest | Listable | Locked | NHoldAll | NHoldRest | NumericFunction
  | OneIdentity | Orderless | Protected | SequenceHold | Stub | Temporary
  deriving Eq

data Definition = Definition {
    attributes :: IORef [Attribute]
  , owncode    :: Maybe (Kernel Expression)
  , upcode     :: Maybe (Expression -> Kernel Expression)
  , subcode    :: Maybe (Expression -> Kernel Expression)
  , downcode   :: Maybe (V.Vector Expression -> Kernel Expression)
  }

emptyDef :: Kernel Definition
emptyDef = do
  attr <- lift $ newIORef []
  return $ Definition attr Nothing Nothing Nothing Nothing

data Environment = Environment {
    iterationLimit :: IORef Int
  , recursionLimit :: IORef Int
  , context        :: IORef String
  , contextPath    :: IORef [String]
  , symbolTable    :: IORef (Map.Map (ContextName, SymbolName) Symbol)
  , builtinDefs    :: Array B.BuiltinSymbol Definition
  , defs           :: MV.IOVector Definition
  }

getDef :: Symbol -> Kernel Definition
getDef symb = do
  env <- ask
  case symb of
    Builtin s        -> return $ (builtinDefs env) ! s
    UserSymbol i _ _ -> lift $ MV.read (defs env) i

-- get symbol from name
getCurrentContext :: Kernel String
getCurrentContext = ask >>= lift . readIORef . context

getIterationLimit :: Kernel Int
getIterationLimit = ask >>= lift . readIORef . iterationLimit

getRecursionLimit :: Kernel Int
getRecursionLimit = ask >>= lift . readIORef . recursionLimit

getContextPath :: Kernel [String]
getContextPath = ask >>= lift . readIORef . contextPath

getTotalName :: String -> Kernel (ContextName, SymbolName)
getTotalName ('`':name) = do current <- getCurrentContext
                             getTotalName (current ++ name)
getTotalName name = case splitWhen (== '`') name of
  []  -> error "unreachable: void symbol name"
  [s] -> do path <- getContextPath
            current <- getCurrentContext
            env <- ask
            tbl <- lift $ readIORef (symbolTable env)
            let c = find (\x -> isJust $ Map.lookup (x, s) tbl) path
            return $ maybe (current, s) ((,) s) c
  cs  -> return ((intercalate "`" (init cs)) ++ "`", last cs)

getSymbol :: String -> Kernel Symbol
getSymbol name = do
  (c, s) <- getTotalName name
  env <- ask
  tbl <- lift $ readIORef (symbolTable env)
  maybe (createSymbol (c, s)) return (Map.lookup (c, s) tbl)

createSymbol :: (ContextName, SymbolName) -> Kernel Symbol
createSymbol (c, s) = do
  env <- ask
  let symbolDefs = defs env
  idNumber <- lift $ MV.length symbolDefs
  let symb = UserSymbol idNumber c s
  emptyDef >>= MV.pushBack symbolDefs
  lift $ modifyIORef (symbolTable env) (Map.insert (c, s) symb)
  return symb

hasAttribute :: Symbol -> Attribute -> Kernel Bool
x `hasAttribute` att = do
  def <- getDef x
  atts <- lift $ readIORef (attributes def)
  return $ att `elem` atts
\end{code}

\end{document}
