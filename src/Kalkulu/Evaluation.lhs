\documentclass[main.tex]{subfiles}

\begin{document}

\chapter{Monadic evaluation}

This chapter describes the module \inline{Kalkulu.Evaluation}, whose
header is
\begin{code}
{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Evaluation where

import qualified Data.Vector as V

import Data.Array
import Data.IORef
import Data.List (sort)
import Data.Maybe (isJust, fromJust)
import qualified Data.Vector.Mutable as MV
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Kalkulu.VectorPattern

import qualified Kalkulu.Builtin as B
import Kalkulu.Expression
import Kalkulu.Symbol
\end{code}

\section{Kernel}

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

data Environment = Environment {
    moduleNumber   :: IORef Int
  , currentContext :: IORef String
  , builtinDefs    :: Array B.BuiltinSymbol Definition
  , defs           :: MV.IOVector Definition
  }

getDef :: Symbol -> Kernel Definition
getDef symb = do
  env <- ask
  case symb of
    Builtin s        -> return $ (builtinDefs env) ! s
    UserSymbol i _ _ -> lift $ MV.read (defs env) i

hasAttribute :: Symbol -> Attribute -> Kernel Bool
x `hasAttribute` att = do
  def <- getDef x
  atts <- lift $ readIORef (attributes def)
  return $ att `elem` atts
\end{code}

\section{Evaluation}

In \emph{Kalkulu}, \emph{evaluation} is the transformation of
expressions by applying \emph{rules}. We will present the
evaluation procedure on the fly with great detail. First
we start with some basic principles:
\begin{itemize}
\item The evaluation process ends when no more rule can modify an
  expression.
\item Builtin rules are applied after user rules: this allows the
  user to redefine the system.
\item In a composite expression, the head is evaluated first.
\end{itemize}

First, we need a function to control the evaluation flow. The
following function \inline{(==>)} combines two evaluation
strategies. If the first one succeeds in modifying an expression,
then the second evaluation strategy is discarded. The function
\inline{(==>)} differs from the usual composition of Kleisli arrows
\inline{(>=>)}.
\begin{code}
(==>) :: (Expression -> Kernel Expression) -> (Expression -> Kernel Expression)
                                           -> (Expression -> Kernel Expression)
(f ==> g) e = f e >>= g'
  where g' x | x == e    = g x
             | otherwise = return x
\end{code}
\subsection{Evaluation of composite expressions with symbolic head}
We treat the case of a composite expression \verb?h[args..]?, such that
\begin{itemize}
\item the head \verb?h? is a symbol,
\item \verb?h? is a fix point under evaluation.
\end{itemize}
\subsubsection{Processing arguments}
The next step in evaluation is to process the arguments. The way
arguments are treated is governed by the attributes of the symbol
\verb?h?. First, arguments are evaluated. This involves the
attributes \verb?HoldAll?, \verb?HoldFirst?, and \verb?HoldRest?, see
table~\ref{eval:tab:evalAttributes}.
\begin{table}[!ht]
  \centering
  \begin{tabular}{c|l}
    \textbf{Attributes} & \textbf{Meaning} \\ \hline
    \verb?HoldAll? & prevents the evaluation of all arguments \\
    \verb?HoldFirst? & prevents the evaluation of the first argument \\
    \verb?HoldRest? & prevents the evaluation of all arguments but the first one
  \end{tabular}
  \caption{Attributes governing evaluation of arguments}
  \label{eval:tab:evalAttributes}
\end{table}
\begin{code}
evalArgs :: Symbol -> V.Vector Expression -> Kernel (V.Vector Expression)
evalArgs h args = do
  hasHoldAll   <- h `hasAttribute` HoldAll
  hasHoldFirst <- h `hasAttribute` HoldFirst
  hasHoldRest  <- h `hasAttribute` HoldRest
  case (hasHoldAll, hasHoldFirst, hasHoldRest) of
    (True, _, _) -> return args
    (_, True, _) -> evalFirst args
    (_, _, True) -> evalRest  args
    _            -> evalAll   args
  where
    evalFirst [] = return V.empty
    evalFirst es = V.cons <$> (evaluate $ V.head es)
                          <*> (pure $ V.tail es)
    evalRest []  = return V.empty
    evalRest es  = V.cons <$> (pure $ V.head es)
                          <*> (V.mapM evaluate $ V.tail es)
    evalAll      = V.mapM evaluate
\end{code}
However, one can force the evaluation of one argument with
\verb?Evaluate?, as shown in the following snippet.
\begin{verbatim}
In[1]:= Hold[Evaluate[2 + 2], 2 + 2]
Out[1]= Hold[4, 2 + 2]
\end{verbatim}
\begin{code}
forceEval :: V.Vector Expression -> Kernel (V.Vector Expression)
forceEval args = V.mapM f args
  where f e@(CmpB B.Evaluate _) = evaluate e
        f e = return e
\end{code}
Then, the arguments are flattened. For example, the expression
\verb?Plus[Plus[a, b], c]? becomes \verb?Plus[a, b, c]? because
\verb?Plus? is \verb?Flat?. Apart from the case of \verb?Flat?
  symbols, arguments whose head is the builtin symbol \verb?Sequence?
  are usually flattened, unless \verb?h? has the attribute
  \verb?SequenceHold?.
\begin{code}
flattenArgs :: Symbol -> V.Vector Expression -> Kernel (V.Vector Expression)
flattenArgs h args = do
  hs <- forbiddenHeads
  return $ V.concatMap (flatten hs) args
  where forbiddenHeads = do
          hasSequenceHold    <- h `hasAttribute` SequenceHold
          hasFlat            <- h `hasAttribute` Flat
          return $ case (hasSequenceHold, hasFlat) of
            (True, True)   -> [h]
            (True, False)  -> []
            (False, True)  -> [Builtin B.Sequence, h]
            (False, False) -> [Builtin B.Sequence]
        flatten :: [Symbol] -> Expression -> V.Vector Expression
        flatten forbidden (Cmp (Symbol s) es) | s `elem` forbidden = es
        flatten _ e = V.singleton e
\end{code}
The next step is to thread the \verb?h? over lists if \verb?h? has
the attribute \verb?Listable?. The table~\ref{eval:tab:thread} shows
how an expression is evaluated when the head is \verb?Listable?.
\begin{table}[!ht]
  \centering
  \begin{tabular}{c|c|c}
    \textbf{Case} & \textbf{Expression} & \textbf{Evaluation} \\ \hline
    1 & \verb?h[{a, b}, {c, d}]? & \verb?{h[a, c], h[b, d]}? \\
    2 & \verb?h[a, {c, d}]? & \verb?{h[a, c], h[a, d]}? \\
    3 & \verb?h[{a}, {c, d}]? & \verb?h[{a}, {c, d}]?
  \end{tabular}
  \caption{Evaluation with \texttt{Listable} head \texttt{h}}
  \label{eval:tab:thread}
\end{table}
When one of the arguments is not a list, then it is automatically
replicated, as in case 2 of table~\ref{eval:tab:thread}. When the
lists have incompatible shapes (case 3 of
table~\ref{eval:tab:thread}), then a warning is sent and the
expression remains unchanged.
\begin{code}
threadLists :: Symbol -> V.Vector Expression -> Kernel (V.Vector Expression)
threadLists x args = do
  hasListable <- x `hasAttribute` Listable
  if hasListable
    then do
      let lengths = V.filter isJust (V.map length' args)
      case lengths of
        []                    -> return args
        h :< t | all (== h) t -> let l = fromJust h in
          return $ V.map applyHead $ tranpose $ V.map (listify l) args
        _                     -> return args -- TODO sendWarning
    else return args
  where
    length' :: Expression -> Maybe Int
    length' (Cmp (SymbolB B.List) es)   = Just (V.length es)
    length' _                           = Nothing
    listify _ (Cmp (SymbolB B.List) es) = es
    listify n e                         = V.replicate n e
    applyHead = Cmp (Symbol x)
    tranpose = undefined
\end{code}
After that, we check whether \verb?h? has the attribute
\verb?Orderless?. If so, the arguments are sorted according to the
canonical order (see section~\ref{expr:canonical_order}). Typically,
commutative operations (\verb?Plus?, \verb?Times?) have the
\verb?Orderless? attribute.
\begin{code}
sortArgs :: Symbol -> V.Vector Expression -> Kernel (V.Vector Expression)
sortArgs x args = do
  hasOrderless <- x `hasAttribute` Orderless
  if hasOrderless
    then return $ sortVector args
    else return args
  where sortVector = V.fromList . sort . V.toList -- TODO find better
\end{code}
Finally, we piece together our work to make the \inline{processArgs}
function. If the symbol \verb?h? has the attribute
\verb?HoldAllComplete?, then its prevents the processing of the
arguments.
\begin{code}
processArgs :: Symbol -> V.Vector Expression -> Kernel (V.Vector Expression)
processArgs h args = do
  hasHoldAllComplete <- h `hasAttribute` HoldAllComplete
  if hasHoldAllComplete
    then return args
    else process args
  where process = (evalArgs h) >=> forceEval
                               >=> (flattenArgs h)
                               >=> (threadLists h)
                               >=> (sortArgs h)
\end{code}

\subsubsection{Applying rules}

After the processing of the arguments, the rules associated to
the symbol \verb?h? are applied. Priority is given to user-defined
rules over built-in rules.
\begin{code}
applyRules :: Expression -> Kernel Expression
applyRules = applyUserRules ==> applyBuiltinRules
\end{code}
User-defined rules are not implemented yet.
\begin{code}
applyUserRules :: Expression -> Kernel Expression
applyUserRules = return . id -- TODO: modify
\end{code}
As for built-in rules, upcode is executed before downcode.
\begin{code}
applyBuiltinRules :: Expression -> Kernel Expression
applyBuiltinRules = applyUpcode ==> applyDowncode

applyUpcode :: Expression -> Kernel Expression
applyUpcode e@(Cmp _ []) = return e
applyUpcode e@(Cmp _ args) = do
  upcodes <- V.mapM (getUpcode . superHead) args
  foldl1 (==>) upcodes $ e
  where getUpcode (Symbol s) = do
          def <- getDef s
          return $ maybe (return . id) id (upcode def)
        getUpcode _ = return (return . id)
applyUpcode _ = error "unreachable"

applyDowncode :: Expression -> Kernel Expression
applyDowncode e@(Cmp (Symbol x) args) = do
  code <- getDowncode x
  maybe (return e) ($ args) code
  where getDowncode symb = getDef symb >>= return . downcode
applyDowncode _ = error "unreachable"
\end{code}

\subsection{Evaluation of symbols}
Again, user-defined rules are preferred over built-in code.
\begin{code}
applyOwnRules :: Expression -> Kernel Expression
applyOwnRules = applyOwnValue ==> applyOwncode

applyOwnValue :: Expression -> Kernel Expression
applyOwnValue = return . id -- TODO

applyOwncode :: Expression -> Kernel Expression
applyOwncode e@(Symbol s) = do
  def <- getDef s
  maybe (return e) id (owncode def)
applyOwncode _ = error "unreachable"
\end{code}

\subsection{Evaluation of composite expressions with composite heads}
In the case of a multi-composite expression, we look for the rules
attached to its superhead.
\begin{code}
applySubRules :: Expression -> Kernel Expression
applySubRules = applySubValue ==> applySubcode

applySubValue :: Expression -> Kernel Expression
applySubValue = return . id -- TODO

applySubcode :: Expression -> Kernel Expression
applySubcode e = case (superHead e) of
  Symbol s -> do def <- getDef s
                 maybe (return e) ($ e) (subcode def)
  _        -> return e
\end{code}

\section{The evaluation function}
\begin{code}
evaluate :: Expression -> Kernel Expression
evaluate e = do
  -- put e in the trace
  e' <- eval e
  if e == e' then return e else evaluate e'

eval :: Expression -> Kernel Expression
eval (Cmp hd@(Cmp _ _) args) = do
  hd' <- evaluate hd
  e' <- (Cmp hd') <$> (V.mapM evaluate args)
  if hd /= hd'
    then return e'
    else (applySubValue ==> applySubcode) e'
eval (Cmp hd@(Symbol x) args) = do
  hd' <- evaluate hd
  if hd /= hd'
    then return (Cmp hd' args)
    else do e' <- (Cmp hd') <$> (evalArgs x args)
            applyRules e'

eval (Cmp hd args) = Cmp <$> (evaluate hd) <*> (V.mapM evaluate args)

eval e@(Symbol _) = (applyOwnValue ==> applyOwncode) e

eval e = return e
\end{code}
\end{document}
