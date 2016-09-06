\documentclass[main.tex]{subfiles}

\begin{document}

\section{Expression}

This sections describes the module \inline{Kalkulu.Expression} whose
header is
\begin{code}
{-# LANGUAGE PatternSynonyms, OverloadedLists, FlexibleInstances #-}
module Kalkulu.Expression where

import Data.Monoid ((<>))
import Data.List (intercalate)
import qualified Data.Vector as V

import qualified Kalkulu.BuiltinSymbol as B
import Kalkulu.Symbol
import Kalkulu.VectorPattern
\end{code}

\subsection{Representation of expressions}

The definition of \inline{Expression} is straightforward. The only
thing worth noting is that the arguments of a composite expression
are contained in a \inline{V.Vector} to provide fast list
manipulations.
\begin{code}
data Expression =
    Number Integer
  | String String
  | Symbol Symbol
  | Cmp Expression (V.Vector Expression)
  deriving (Eq)
\end{code}
The leftmost atom in an expression is called its \emph{super head}.
\begin{code}
superHead :: Expression -> Expression
superHead (Cmp h _) = superHead h
superHead expr      = expr
\end{code}
Next we make \inline{Expression} an instance of the \inline{Show}
typeclass. This is not strictly necessary, as the burden of printing
expressions is let to the front-end. But there is currently no
front-end...
\begin{code}
instance Show Expression where
  show (Number x) = show x
  show (String x) = show x
  show (Symbol (Builtin x)) = "System`" ++ show x
  show (Symbol (UserSymbol _ x y)) = y ++ x
  show (Cmp h args) = show h ++ "[" ++
    (intercalate ", " $ V.toList (V.map show args)) ++ "]"
\end{code}

\subsection{Pattern synonyms}
Pattern-matching built-in symbols will become a regular task, thus
we useful pattern synonyms.
\begin{code}
pattern SymbolB :: B.BuiltinSymbol -> Expression
pattern SymbolB symb = Symbol (Builtin symb)

pattern CmpB :: B.BuiltinSymbol -> V.Vector Expression -> Expression
pattern CmpB h args = Cmp (SymbolB h) args
\end{code}

\subsection{Canonical order}
\label{expr:canoncical_order}
We define a canonical order for expressions. This order plays an
important role in many parts (see~\verb?Orderless?, \verb?Sort?).

We have the (approximate) ordering
\begin{equation}
  \label{eq:canonical_order}
  \text{Numbers} < \inline{String} < \inline{Symbol} \lesssim
  \text{Composite expressions}.
\end{equation}
For reasons explained below, composite expressions are not always
greater than atoms.  Numbers are ordered by their value, whereas
\inline{String}s are ordered alphabetically.
\begin{code}
instance Ord Expression where compare = compareExpr

compareExpr :: Expression -> Expression -> Ordering
compareExpr (Number x) (Number y)  = compare x y
compareExpr (Number _) (String _)  = LT
compareExpr (Number _) (Symbol _)  = LT
compareExpr (String _) (Number _)  = GT
compareExpr (String x) (String y)  = compare x y
compareExpr (String _) (Symbol _)  = LT
compareExpr (Symbol _) (Number _)  = GT
compareExpr (Symbol _) (String _)  = GT
compareExpr (Symbol x) (Symbol y)  = compare x y
\end{code}
Next, we specify how to compare composite expressions. The general
rule is that~:
\begin{enumerate}
\item we first compare them by their heads,
\item in case of a tie, we compare them by the number of their
  arguments,
\item in case of another tie, we use the lexicographic order to
  compare their arguments.
\end{enumerate}
There are exceptions to the above rule: composite expressions with
head \verb?Times? or \verb?Power? are treated specially because we
want monomials to be ordered strategically. This is important for
polynomial computations. For example, consider two atoms \verb?x? and
\verb?y? (with $\verb?x? < \verb?y?$). Suppose we want to sort the
following expressions
\begin{equation}
\label{eq:canonical_expression2}
  1, \verb?x?, \verb?y?, \verb?Power[x, 2]?, \verb?Times[x, y]?,
  \verb?Power[y, 2]?, \verb?Power[x, y]?
\end{equation}
We rewrite each of them in exponent form:
$(\verb?y?,\verb?x?)^{(e_1,e_2)} = \verb?y?^{e_1} \verb?x?^{e_2}$, and
sort the exponents in the lexicographic order.
\[
  (0,0) < (0,1) < (0,2) < (0, y) < (1,0) < (1,1) < (2,0)
\]
This corresponds to the following ordering of the expressions
in~\eqref{eq:canonical_expression2}:
\[
  1 < \verb?x? < \verb?Power[x, 2]? < \verb?Power[x, y]?
  < \verb?y? < \verb?Times[x, y]? < \verb?Power[y, 2]?
\]
This breaks the rule~\eqref{eq:canonical_order} stating that a symbol
is less than a composite expression (as \verb?y? is greater than
\verb?Power[x, y]?).

First, we begin to treat the cases where the first expression to
compare is a \verb?Power?.
\begin{code}
compareExpr (CmpB B.Power [a, b]) (CmpB B.Power [c, d]) =
  compare a c <> compare b d
\end{code}
What happens when we compare a power \verb?Power[a, b]? with an empty
product? As both \verb?Times[]? and \verb?Power[1, 1]? evaluate to
\verb?1?, we are let to compare again two powers: \verb?Power[a, b]?
and \verb?Power[1, 1]?. In case of a tie, we decrete that
\verb?Power[1, 1]? is less than \verb?Times[]? (in accordance
with the general rule for sorting composite expressions).
\begin{code}
compareExpr (CmpB B.Power [a, b]) (CmpB B.Times []) =
  compare a (Number 1) <> compare b (Number 1) <> LT
\end{code}
To compare a \verb?Power[a,b]? with a product \verb?Times[..., y]?,
we ought to compare it with each of the factors appearing in
\verb?Times[..., y]?. However, the symbol \verb?Times? is by
default \verb?Orderless?, which means that\footnote{unless the
  unwise user turns off the \texttt{Orderless} attribute of
  \texttt{Times} or compares unevaluated expressions.} the greatest
factor is the last one \verb?y?. For efficiency reason, the power is
compared with \verb?y? only.
\begin{code}
compareExpr e1@(CmpB B.Power [_, _]) (CmpB B.Times ys) =
  compare e1 (V.last ys) <> LT
\end{code}
When we compare \verb?Power[a, b]? with an expression
\verb?e2? (which is neither a \verb?Power? nor a \verb?Times?),
the idea is to treat \verb?e2? as if it were \verb?Power[e2, 1]?,
to reduce to the case where we compared two \verb?Power?s.
\begin{code}
compareExpr (CmpB B.Power [a, b]) e2 =
  compare a e2 <> compare b (Number 1) <> GT -- GT correct?
\end{code}
Next we treat the symmetrical cases where the second expression is a
\verb?Power?.
\begin{code}
compareExpr (CmpB B.Times []) (CmpB B.Power [a, b]) =
  compare (Number 1) a <> compare (Number 1) b <> GT
compareExpr (CmpB B.Times xs) e2@(CmpB B.Power [_, _]) =
  compare (V.last xs) e2 <> GT
compareExpr e1 (CmpB B.Power [c, d]) =
  compare e1 c <> compare (Number 1) d <> LT
\end{code}
To compare two products, we compare their greatest factors, which
should be at the end of the argument lists (because \texttt{Times} is
\texttt{Orderless}).
\begin{code}
compareExpr (CmpB B.Times []) (CmpB B.Times []) = EQ
compareExpr (CmpB B.Times xs) (CmpB B.Times []) =
  compare (V.last xs) (Number 1) <> GT
compareExpr (CmpB B.Times []) (CmpB B.Times (_ :> y)) =
  compare (Number 1) y <> LT
compareExpr (CmpB B.Times (xs :> x)) (CmpB B.Times (ys :> y)) =
  compare x y <> compare (CmpB B.Times xs) (CmpB B.Times ys)
\end{code}
The expression \verb?Times[]? evaluates to \verb?1?, which motivates
the following choice:
\begin{code}
compareExpr (CmpB B.Times []) y = compare (Number 1) y <> GT
compareExpr x (CmpB B.Times []) = compare x (Number 1) <> LT
\end{code}
When we compare a non void product \verb?Times[..., x]? with a
(non product) expression \verb?y?, we compare the last (and
supposedly greatest) factor \verb?x? with \verb?y?.
\begin{code}
compareExpr (CmpB B.Times xs) y = compare (V.last xs) y <> GT
compareExpr x (CmpB B.Times ys) = compare x (V.last ys) <> LT
\end{code}
Finally, the general rule to compare composite expressions
is translated below.
\begin{code}
compareExpr (Cmp h1 args1) (Cmp h2 args2) = (compare h1 h2)
  <> (compare (length args1) (length args2))
  <> (compare args1 args2)
\end{code}
Composite expressions are greater than atoms, except in some cases
treated above.
\begin{code}
compareExpr (Cmp _ _) _         = GT
compareExpr _         (Cmp _ _) = LT
\end{code}

\begin{code}
class ToExpression a where
  toExpression :: a -> Expression

instance ToExpression Bool where
  toExpression True  = SymbolB B.True
  toExpression False = SymbolB B.False

instance ToExpression Integer where
  toExpression = Number

instance ToExpression String where
  toExpression = String

instance ToExpression Expression where
  toExpression = id

instance ToExpression B.BuiltinSymbol where
  toExpression = SymbolB

instance ToExpression Symbol where
  toExpression = Symbol
\end{code}
\end{document}
