\documentclass[main.tex]{subfiles}

\begin{document}
\chapter{Pattern matching}

This chapter describes the module \inline{Kalkulu.Pattern}, with the
following header.
\begin{code}
-- {-# LANGUAGE OverloadedLists #-}
module Kalkulu.Pattern where

import Kalkulu.Expression
import Kalkulu.Kernel
import Kalkulu.Symbol
import qualified Kalkulu.BuiltinSymbol as B
import Control.Monad
import Control.Monad.Trans
import qualified Data.Vector as V
\end{code}

\section{Patterns}

\subsection{Representation of patterns}

In \emph{Kalkulu}, any object is necessarily represented by an
expression. This holds in particular for patterns, which are special
expressions representing class of expressions. For example, the
pattern \verb?_? represents any expression, whereas the pattern
\verb?f[___]? stands for any expression whose head is
\verb?f?. Although patterns are entered as expressions inside the
\emph{Kalkulu} interpreter, it is clearer if we make a distinct data
type \inline{Pattern}, with the possibility to convert a
\inline{Pattern} to an \inline{Expression}.
\begin{code}
data Pattern =
    Blank                                       -- _
  | BlankSequence                               -- __
  | BlankNullSequence                           -- ___
  | HeadedBlank             Expression          -- e_
  | HeadedBlankSequence     Expression          -- e__
  | HeadedBlankNullSequence Expression          -- e___
  | Pattern                 Symbol Pattern      -- s:p
  | Alternative             [Pattern]           -- a|b|c|..
  | Optional1               Pattern             -- Optional[p]
  | Optional2               Pattern Expression  -- Optional[p, e]
  | Expression              Expression          -- e
  | PatternCmp              Pattern [Pattern]   -- p[p1, ..]
\end{code}
Most of the above constructors should be familiar to the
\emph{Kalkulu} user: the corresponding \emph{Kalkulu} expression is
shown in comments). The constructor \inline{PatternCmp} builds
composite patterns in the same way that \inline{Cmp} builds composite
expressions. As an example, the pattern \verb?f[_]? is represented by
\inline{PatternCmp (Expression (Symbol f) [Blank])}.

At first sight, it seems that there two ways to translate \verb?a+b?
as a pattern, either with the constructor \verb?Expression?, or
with the constructor \verb?PatternCmp?. Those solutions are
distinct:

\begin{itemize}
  \item The first pattern, built with \inline{Expression}, does not
    match with \verb?b+a?: in order to match with a pattern of type
    \inline{Expression}, an expression is required to be equal to the
    pattern.
  \item The second pattern, built with \inline{PatternCmp}, matches
    with \verb?b+a?: because the pattern matcher takes into account
    the associativity of \verb?Plus?.
\end{itemize}

\subsection{Conversions between expressions and patterns}

Of course, patterns can be converted back to expressions. One has to
be careful when translating patterns to expressions: for example
\inline{Expression (CmpB B.Blank [])} should not sent to the same
expression as the pattern \inline{Blank :: Pattern} (the
\emph{Kalkulu} expression \verb?_?). Instead, the first pattern
is mapped to \verb?Verbatim[_]?, where the builtin symbol
\verb?Verbatim? requires that \verb?_? is matched as it appears, with
no subsitutions.
\begin{code}
instance ToExpression Pattern where
  toExpression Blank                   = CmpB B.Blank V.empty
  toExpression BlankSequence           = CmpB B.BlankSequence V.empty 
  toExpression BlankNullSequence       = CmpB B.BlankNullSequence V.empty
  toExpression (HeadedBlank h)         = CmpB B.Blank (V.singleton h)
  toExpression (HeadedBlankSequence h) = CmpB B.BlankSequence (V.singleton h)
  toExpression (HeadedBlankNullSequence h) =
    CmpB B.BlankNullSequence (V.singleton h)
  toExpression (Pattern s p) =
    CmpB B.Pattern (V.fromList [toExpression s, toExpression p])
  toExpression (Alternative es) =
    CmpB B.Alternative (V.fromList $ map toExpression es)
  toExpression (Optional1 p) = CmpB B.Optional (V.singleton $ toExpression p)
  toExpression (Optional2 p e) = CmpB B.Optional (V.fromList [toExpression p, e])
  toExpression (PatternCmp h as) =
    Cmp (toExpression h) (V.fromList $ map toExpression as)
  toExpression (Expression e) = e -- TODO: add Verbatim
\end{code}

\section{What is pattern matching?}
Pattern matching involves an expression and a pattern.
Basically, the purpose of pattern matching is to decide whether the
expression belongs to the class of expressions represented by the
pattern. This aspect of pattern matching is covered by the built-in
function \verb?MatchQ[expr, pattern]?.
\begin{verbatim}
In[1]:= MatchQ[a+b, a+_]
Out[1]= True

In[2]:= MatchQ[a+b, A+_]
Out[2]= False
\end{verbatim}
One can be more precise by exhibiting the relevant bindings when a
pattern matches an expression.

\begin{table}[!h]
  \centering
  \begin{tabular}{c|c|c}
    \textbf{Expression} & \textbf{Pattern} & \textbf{Bindings} \\ \hline
    \verb?f[a,b]? & \verb?_[x_,y_]? & \texttt{[(x,a), (y,a)]} \\ \hline
    \multirow{2}{*}{\texttt{f[a,b,c]}} &
    \multirow{2}{*}{\texttt{f[x\_\_, y\_\_]}} &
    \texttt{[(x,Sequence[a]), (y,Sequence[b,c])]} \\
    & & \texttt{[(x, Sequence[a, b]), (y, Sequence[c])]} \\ \hline
    \verb?f[a,f[a,b,c]]? & \verb?f[x__,f[x__,y__]]? &
    \texttt{[(x,Sequence[a]), (y,Sequence[b,c])]}
  \end{tabular}
  \caption{Some examples of pattern matching}
  \label{pat:tab:examples}
\end{table}

Table~\ref{pat:tab:examples} teaches us some interesting facts. First,
it is natural to design a recursive pattern matcher, because patterns
can be nested arbitrarily deep. But cases 2 and 3 show that it is then
necessary to solve a somewhat more general problem: pattern matching a
\emph{list} of expressions with a \emph{list} of patterns. The second
case also shows that the pattern matcher can yield more than one
solution. The third case shows that all solutions must be taken into
account in case of backtracking. Moreover, when calling recursively
the pattern matching, one has to respect past bindings.

Table~\ref{pat:tab:binding} demonstrates that the binding depends not
only on the expression, but also on the pattern, and on some extra
information concerning the underlying symbol head.
\begin{table}[!h]
  \centering
  \begin{tabular}{c|c|c|c}
    \textbf{Expression} & \textbf{Pattern} & \textbf{Binding} &
    \textbf{Context} \\ \hline
    \multirow{5}{*}{\texttt{f[a]}} & \verb?f[x_]? & \verb?[(x, a)]? & $\emptyset$\\
    & \verb?f[x_]? & \verb?[(x, f[a])]? & \verb?f? is \verb?Flat? \\
    & \verb?f[x_]? & \verb?[(x, a)]? & \verb?f? is \verb?Flat? and
    \verb?OneIdentity? \\
    & \verb?f[x__]? & \verb?[(x, Sequence[a])]? & $\emptyset$ \\
    & \verb?f[x___]? & \verb?[(x, Sequence[a])]? & $\emptyset$ \\
  \end{tabular}
  \caption{Pattern matching with expression \texttt{f[a]}}
  \label{pat:tab:binding}
\end{table}
\section{Bindings}
The pattern \verb?f[x_]? matches the expression \verb?f[a]? and binds
the \emph{symbol} \verb?x? to the \emph{expression} \verb?a?. More
generally, symbols can be bound to either expressions or sequence of
expressions. For that, we introduce the type
\inline{BoundExpression}.
\begin{code}
data BoundExpression = Unique Expression
                     | Sequence [Expression]
\end{code}
Several symbols can be bound to \inline{BoundExpression}s. The result
of pattern matching is an association list.
\begin{code}
type Bindings = [(Symbol, BoundExpression)]
\end{code}
The pattern matcher is based on two mutually recursive functions,
\inline{match} and \inline{matchMany}, which respectively match a single
pattern or a list of patterns against a list of expressions.
\begin{spec}
match ::  [Expression] -> Pattern -> Maybe Symbol
                       -> Bindings -> Kernel [] Bindings, BoundExpression)
\end{spec}
The third argument of type \inline{Maybe Symbol} is the preexisting
head (if it exists). For example, matching \verb?f[x_]? against
\verb?f[expr1, expr2]? eventually calls \inline{match} with the
arguments \verb?[expr1, expr2]?, \verb?x_? and \inline{Just f}.
The fourth argument of type \inline{Bindings} represents a set of
\emph{required} bindings, which have to be respected. Finally, the
result lies in the \inline{Kernel} monad (because we need to check
the attributes of the underlying head). Because of the non determinism,
we return a \emph{list} of \inline{(Bindings, BoundExpression)}, where
the first element of type \inline{Bindings} represents the new bindings,
and the second element is the expression bound to the pattern.

The second function has the following signature.
\begin{spec}
matchMany :: [Expression] -> [Pattern] -> Maybe Symbol
                                       -> Bindings -> Kernel [] Bindings
\end{spec}

\begin{code}
insert :: Bindings -> (Symbol, BoundExpression) -> Maybe Bindings
insert [] x = Just [x]
insert assoc@(bind@(s, e) : bs) new@(s', e')
  | s /= s'   = insert bs (s', e') >>= return . ((:) bind)
  | otherwise = case (e, e') of
      (Unique e1, Unique e2)      | e1 == e2 -> Just assoc
      (Unique e1, Sequence[e2])   | e1 == e2 -> Just assoc
      (Sequence[e1], Unique e2)   | e1 == e2 -> Just (new : bs)
      (Sequence e1, Sequence e2)  | e1 == e2 -> Just assoc
      _                                      -> Nothing
\end{code}

\section{The pattern matcher}
First, we match a list of expressions with the pattern \verb?_?.  A
\verb?_? cannot match an empty expression sequence. To denote failure,
we return an empty list.
\begin{code}
match :: [Expression] -> Pattern -> Maybe Symbol -> Bindings
                                 -> KernelT [] (Bindings, BoundExpression)
match [] Blank _ _ = mzero
match [e] Blank (Just s) req = do
  hasFlat <- s `hasAttribute` Flat
  hasOneIdentity <- s `hasAttribute` OneIdentity
  return $ if hasFlat && not hasOneIdentity
    then (req, Unique (Cmp (Symbol s) (V.singleton e)))
    else (req, Unique e)
match [e] Blank Nothing req = return (req, Unique e)
match _ Blank Nothing _ = mzero
match es Blank (Just s) req = do
  hasFlat <- s `hasAttribute` Flat
  if hasFlat
    then return (req, Unique (Cmp (Symbol s) (V.fromList es)))
    else mzero
\end{code}

\begin{code}
match []  (HeadedBlank _) _ _ = mzero
match [e] (HeadedBlank h) (Just s) req = do
  hasFlat <- s `hasAttribute` Flat
  hasOneIdentity <- s `hasAttribute` OneIdentity
  let h' = Symbol s
  case hasFlat && not hasOneIdentity of
    True  -> if getHead e == h'
      then return (req, Unique (Cmp h' $ V.singleton e))
      else mzero
    False -> if getHead e == h then return (req, Unique e) else mzero
match [e] (HeadedBlank h) Nothing req
  | getHead e == h = return (req, Unique e)
  | otherwise      = mzero
match es (HeadedBlank h) (Just s) req = do
  hasFlat <- s `hasAttribute` Flat
  let h' = Symbol s
  if hasFlat && h == h'
    then return (req, Unique (Cmp h' (V.fromList es)))
    else mzero
match _ (HeadedBlank _) Nothing _ = mzero
\end{code}

\begin{code}
match []  BlankSequence _ _ = mzero
match es BlankSequence _ req = return (req, Sequence es)
\end{code}

\begin{code}
match [] (HeadedBlankSequence _) _ _ = mzero
match es (HeadedBlankSequence h) _ req
  | all (== h) (map getHead es) = return (req, Sequence es)
  | otherwise                   = mzero
\end{code}

\begin{code}
match es BlankNullSequence _ req = return (req, Sequence es)
match es (HeadedBlankNullSequence h) _ req
  | all (== h) (map getHead es) = return (req, Sequence es)
  | otherwise                   = mzero
\end{code}

\begin{code}
match es (Pattern symb p) lhs req = do
  (bindings, e) <- match es p lhs req
  let Just b = insert bindings (symb, e)
  return (b, e)
\end{code}

\begin{code}
match es (Alternative ps) lhs req =
  -- msum [match es p lhs req | p <- ps]
  do p <- liftK ps
     match es p lhs req
  where liftK = undefined
\end{code}
\end{document}
The only way for an expression to match with a pattern
\inline{Expression e'} is to coincide with \verb?e'?.
\begin{code}
match [e] (Expression e') _ req
  | e == e'   = return (req, Unique e)
match _ (Expression _) _ _ = mzero
\end{code}

\begin{code}
match es (Optional1 p) Nothing req = match es p Nothing req
match es (Optional1 pat@(Pattern s _)) (Just s') req =
  bindings `mplus` optionalBindings
  where bindings = match es pat (Just s') req
        optionalBindings = do Just def <- getDefault s'
                              let Just req' = insert req (s, Unique def)
                              return (req', Unique def)
match es (Optional1 p) (Just s) req =
  bindings `mplus` optionalBindings
  where bindings = match es p (Just s) req
        optionalBindings = do Just def <- getDefault s
                              return (req, Unique def)
\end{code}

\begin{code}
match es (Optional2 p def) lhs req =
  match es p lhs req `mplus` return (req, Unique def)
\end{code}

To change (\verb?MatchQ[b, a_. + b] == True?).
\begin{code}
match [e@(Cmp h as)] (PatternCmp h' ps) _ req = do
   (req', _)  <- match [h] h' Nothing req
   req'' <- matchMany (V.toList as) ps Nothing req'
   return (req'', Unique e)
match _ (PatternCmp _ _) _ _ = mzero
\end{code}


\begin{code}
matchMany :: [Expression] -> [Pattern] -> Maybe Symbol -> Bindings
                          -> KernelT [] Bindings
matchMany = undefined
\end{code}
%findMatches es [Pattern s p] lhs req = do
%  matches <- findMatches es [p] lhs req
%  return undefined
%\end{code}
