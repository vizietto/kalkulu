\documentclass[main.tex]{subfiles}

\begin{document}
\chapter{Pattern matching}

This chapter describes the module \inline{Kalkulu.Pattern}, with the
following header.
\begin{code}
{-# LANGUAGE OverloadedLists #-}
module Kalkulu.Pattern where

import Kalkulu.Expression
import Kalkulu.Kernel
import Kalkulu.Symbol
import qualified Kalkulu.BuiltinSymbol as B
import qualified Data.Vector as V
\end{code}

\section{Representation of patterns}

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
data Pattern = Blank                                     -- _
             | BlankSequence                             -- __
             | BlankNullSequence                         -- ___
             | HeadedBlank             Expression        -- e_
             | HeadedBlankSequence     Expression        -- e__
             | HeadedBlankNullSequence Expression        -- e___
             | Pattern                 Symbol Pattern    -- s:p
             | Alternative             [Pattern]         -- a|b|c|..
             | Expression              Expression        -- e
             | PatternCmp              Pattern [Pattern] -- p[p1, ..]
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
  toExpression Blank                   = CmpB B.Blank []
  toExpression BlankSequence           = CmpB B.BlankSequence []
  toExpression BlankNullSequence       = CmpB B.BlankNullSequence []
  toExpression (HeadedBlank h)         = CmpB B.Blank [h]
  toExpression (HeadedBlankSequence h) = CmpB B.BlankSequence [h]
  toExpression (HeadedBlankNullSequence h) = CmpB B.BlankNullSequence [h]
  toExpression (Pattern s p) =
    CmpB B.Pattern [toExpression s, toExpression p]
  toExpression (Alternative es) =
    CmpB B.Alternative (V.fromList $ map toExpression es)
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
One can be more precise by exhibiting the relevant bindings
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
\begin{code}
type Association = [(Symbol, Expression)]

-- f Flat
-- MatchQ[f[], f[z_]]          ===> False
-- MatchQ[f[x, y], f[z_]]      ===> True (binding x -> Sequence[x, y]

-- req: required bindings
findMatches :: [Expression] -> [Pattern] -> (Maybe Symbol) -> [Association] -> Kernel [Association]
findMatches [] [] _ req = return req
findMatches _  [] _ req = return []
-- Matching with _
findMatches []  [Blank] _ _          = return []  
findMatches [e] [Blank] _ req        = return req
findMatches _   [Blank] Nothing _    = return []
findMatches _   [Blank] (Just s) req = do
  hasFlat <- s `hasAttribute` Flat
  return $ if hasFlat then req else []
findMatches []  [HeadedBlank _] _ _   = return []
findMatches [e] [HeadedBlank h] _ req
  | getHead e == h = return req
  | otherwise      = return []
findMatches _   [HeadedBlank _] Nothing _    = return []
findMatches es  [HeadedBlank h] (Just s) req = do
  hasFlat <- s `hasAttribute` Flat
  return $ if hasFlat && all (== h) (map getHead es) then req else []
-- Matching with __
findMatches [] [BlankSequence] _ _                  = return []
findMatches _  [BlankSequence] _ req                = return req
findMatches [] [HeadedBlankSequence _] _ _          = return []
findMatches es [HeadedBlankSequence h] (Just s) req = do
  hasFlat <- s `hasAttribute` Flat
  return $ if hasFlat && all (== h) (map getHead es) then req else []
-- Matching with ___
findMatches _  [BlankNullSequence] _ req         = return req
findMatches es [HeadedBlankNullSequence h] _ req =
  return $ if all (== h) (map getHead es) then req else []
-- Matching with Pattern[s, p]
findMatches es [Pattern s p] lhs req = do
  matches <- findMatches es [p] lhs req
  return undefined
              
       
\end{code}

\end{document}
