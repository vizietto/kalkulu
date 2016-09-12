\documentclass[main.tex]{subfiles}

\begin{document}

\chapter{Kernel}

This chapter describes the module \inline{Kalkulu.Kernel}, whose
header is
\begin{code}
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Kalkulu.Kernel where

import Data.List (intercalate)
import Data.List.Split (splitWhen)
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Trans.Writer

import qualified Kalkulu.BuiltinSymbol as B
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
  deriving (Bounded, Enum, Eq)

instance ToExpression Attribute where
  toExpression Constant        = toExpression B.Constant
  toExpression Flat            = toExpression B.Flat
  toExpression HoldAll         = toExpression B.HoldAll
  toExpression HoldAllComplete = toExpression B.HoldAllComplete
  toExpression HoldFirst       = toExpression B.HoldFirst
  toExpression HoldRest        = toExpression B.HoldRest
  toExpression Listable        = toExpression B.Listable
  toExpression Locked          = toExpression B.Locked
  toExpression NHoldAll        = toExpression B.NHoldAll
  toExpression NHoldRest       = toExpression B.NHoldRest
  toExpression NumericFunction = toExpression B.NumericFunction
  toExpression OneIdentity     = toExpression B.OneIdentity
  toExpression Orderless       = toExpression B.Orderless
  toExpression Protected       = toExpression B.Protected
  toExpression SequenceHold    = toExpression B.SequenceHold
  toExpression Stub            = toExpression B.Stub
  toExpression Temporary       = toExpression B.Temporary

data BuiltinCode = BuiltinCode {
    owncode    :: Maybe (Kernel Expression)
  , upcode     :: Expression -> Kernel Expression
  , subcode    :: Expression -> Kernel Expression
  , downcode   :: Expression -> Kernel Expression
  }

data Action a =
    GetSymbolMaybe    ContextName SymbolName (Maybe Symbol -> a)
  | CreateSymbol      ContextName SymbolName (Symbol -> a)
  | GetBuiltinCode    Symbol (BuiltinCode -> a)
  | GetDefault        Symbol (Maybe Expression -> a)
  | HasAttribute      Symbol Attribute          (Bool -> a)
  | GetIterationLimit (Maybe Int -> a)
  | SetIterationLimit (Maybe Int) a
  | GetRecursionLimit (Maybe Int -> a)
  | SetRecursionLimit (Maybe Int) a
  | GetCurrentContext (String -> a)
  | GetContextPath    ([String] -> a)
  deriving Functor

class Monad m => MonadEnv m where
  getSymbolMaybe    :: ContextName -> SymbolName -> m (Maybe Symbol)
  createSymbol      :: ContextName -> SymbolName -> m Symbol
  getBuiltinCode    :: Symbol -> m BuiltinCode
  getDefault        :: Symbol -> m (Maybe Expression)
  hasAttribute      :: Symbol -> Attribute -> m Bool
  getIterationLimit :: m (Maybe Int)
  setIterationLimit :: Maybe Int -> m ()
  getRecursionLimit :: m (Maybe Int)
  setRecursionLimit :: Maybe Int -> m ()
  getCurrentContext :: m String
  getContextPath    :: m [String]

instance (Monoid w, MonadEnv m) => MonadEnv (WriterT w m) where
  getSymbolMaybe c s = lift $ getSymbolMaybe c s
  createSymbol   c s = lift $ createSymbol c s
  getBuiltinCode     = lift . getBuiltinCode
  getDefault s       = lift $ getDefault s
  hasAttribute s a   = lift $ hasAttribute s a
  getIterationLimit  = lift getIterationLimit
  setIterationLimit  = lift . setIterationLimit
  getRecursionLimit  = lift getRecursionLimit
  setRecursionLimit  = lift . setRecursionLimit
  getCurrentContext  = lift getCurrentContext
  getContextPath     = lift getContextPath

instance Monad m => MonadEnv (FreeT Action m) where
  getSymbolMaybe c s  = liftF $ GetSymbolMaybe c s id
  createSymbol c s    = liftF $ CreateSymbol c s id
  getBuiltinCode s    = liftF $ GetBuiltinCode s id
  getDefault s        = liftF $ GetDefault s id
  hasAttribute s at   = liftF $ HasAttribute s at id
  getIterationLimit   = liftF $ GetIterationLimit id
  setIterationLimit l = liftF $ SetIterationLimit l ()
  getRecursionLimit   = liftF $ GetRecursionLimit id
  setRecursionLimit l = liftF $ SetRecursionLimit l ()
  getCurrentContext   = liftF $ GetCurrentContext id
  getContextPath      = liftF $ GetContextPath id

getAttributes :: MonadEnv m => Symbol -> m [Attribute]
getAttributes s = filterM (s `hasAttribute`) allAttributes
  where allAttributes = [minBound ..]

getTotalName :: MonadEnv m => String -> m (ContextName, SymbolName)
getTotalName ('`':name) = do current <- getCurrentContext
                             getTotalName (current ++ name)
getTotalName name = case splitWhen (== '`') name of
  []  -> error "unreachable: void symbol name"
  [s] -> do path <- getContextPath
            current <- getCurrentContext
            -- (\x -> isJust $ getSymbolMaybe x s) path
            find' path s >>= (return . (maybe (current, s) (flip (,) s)))
  cs  -> return ((intercalate "`" (init cs)) ++ "`", last cs)
  where find' [] _     = return Nothing
        find' (c:cs) s = getSymbolMaybe c s >>=
          maybe (find' cs s) (const $ return (Just c))

getSymbol :: MonadEnv m => String -> m Symbol
getSymbol name = do
  (c, s) <- getTotalName name
  -- TODO: send message if newsym or shadow
  getSymbolMaybe c s >>= maybe (createSymbol c s) return

getOwncode :: MonadEnv m => Symbol -> m (Maybe (Kernel Expression))
getOwncode s = getBuiltinCode s >>= return . owncode

getUpcode :: MonadEnv m => Symbol -> m (Expression -> Kernel Expression)
getUpcode s = getBuiltinCode s >>= return . upcode

getDowncode :: MonadEnv m => Symbol -> m (Expression -> Kernel Expression)
getDowncode s = getBuiltinCode s >>= return . downcode

getSubcode :: MonadEnv m => Symbol -> m (Expression -> Kernel Expression)
getSubcode s = getBuiltinCode s >>= return . subcode
\end{code}

\section{Evaluation trace}

\begin{code}
data LogExpression = LogItem     Expression
                   | LogSequence [LogExpression]

pack :: [LogExpression] -> [LogExpression]
pack s = [LogSequence s]

type KernelT m a = WriterT [LogExpression] (FreeT Action m) a

type Kernel a = KernelT Identity a 

newtype RecursionLevel = RecursionLevel Int

instance Monoid RecursionLevel where
  mempty = RecursionLevel 0
  (RecursionLevel n) `mappend` (RecursionLevel m) = RecursionLevel (n+m)

\end{code}

\end{document}
