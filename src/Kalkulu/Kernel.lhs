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

\section{Introduction}

The evaluation function, that will be defined in the next chapter, is
not a pure function of type \inline{Expression -> Expression}, because
in an interpreted language such as \emph{Kalkulu}, evaluation is
fundamentally impure. Instead, it has type
\inline{Expression -> Kernel Expression}, where the type
constructor \inline{Kernel} is the subject of this
chapter. \inline{Kernel} is defined to be a monad, and as such, it
easily combines a lot of functionalities, under which
\begin{enumerate}
\item the ability to perform some IO actions, like opening a file,
  reading the time, etc.
\item the maintenance of a huge state: definitions for each symbol,
  configuration variables, etc.
\item communication with the front-end: not all IO actions are
  performed by the kernel, some (like printing) are left to the
  front-end,
\item the management of messages (the \emph{Kalkulu} term for warnings),
\item controlling evaluation: stopping infinite loops,
\item storing past evaluations for debbuging purposes.
\end{enumerate}

\section{Environment}

\begin{code}
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

emptyDef :: IO Definition
emptyDef = do
  attr <- newIORef []
  return $ Definition attr Nothing Nothing Nothing Nothing

data Infinitable a = Finite a | Infinity

data Environment = Environment {
    iterationLimit :: IORef (Infinitable Int)
  , recursionLimit :: IORef (Infinitable Int)
  , context        :: IORef String
  , contextPath    :: IORef [String]
  , symbolTable    :: IORef (Map.Map (ContextName, SymbolName) Symbol)
  , builtinDefs    :: Array B.BuiltinSymbol Definition
  , defs           :: MV.IOVector Definition
  }

defaultEnv :: IO Environment
defaultEnv = Environment
  <$> newIORef (Finite 4096) -- iteration limit
  <*> newIORef (Finite 1024) -- recursion limit
  <*> newIORef "Global`"              -- current context
  <*> newIORef ["System`", "Global`"] -- context path
  <*> newIORef (Map.fromList [(("System`", show b), Builtin b) -- symbolTable
                                            | b <- [minBound..]])
  <*> (array (minBound, maxBound) <$> builtin)
  <*> (MV.new 0)
  where builtin :: IO [(B.BuiltinSymbol, Definition)]
        builtin = sequence [(,) b <$> emptyDef | b <- [minBound..]]

type Kernel = ReaderT Environment IO

getDef :: Symbol -> Kernel Definition
getDef symb = do
  env <- ask
  case symb of
    Builtin s        -> return $ (builtinDefs env) ! s
    UserSymbol i _ _ -> lift $ MV.read (defs env) i

getCurrentContext :: Kernel String
getCurrentContext = ask >>= lift . readIORef . context

-- Nothing means Infinity
getIterationLimit :: Kernel (Infinitable Int)
getIterationLimit = ask >>= lift . readIORef . iterationLimit

getRecursionLimit :: Kernel (Infinitable Int)
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
  let symb = UserSymbol idNumber s c
  (lift emptyDef) >>= MV.pushBack symbolDefs
  lift $ modifyIORef (symbolTable env) (Map.insert (c, s) symb)
  return symb

hasAttribute :: Symbol -> Attribute -> Kernel Bool
x `hasAttribute` att = do
  def <- getDef x
  atts <- lift $ readIORef (attributes def)
  return $ att `elem` atts
\end{code}

\end{document}
