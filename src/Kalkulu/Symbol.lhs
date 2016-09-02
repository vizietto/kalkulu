\documentclass[kalkulu.tex]{subfiles}
\begin{document}

\section{Symbols}
This section describes the module \inline{Symbol}. We begin by a
series of \inline{import}s.
\begin{code}
module Kalkulu.Symbol(Symbol(..)) where

import qualified Kalkulu.Builtin as B
import Data.Monoid ((<>))
\end{code}
We define the \inline{Symbol} type, to represent \emph{Kalkulu}'s
symbolic atoms. Built-in symbols are treated differently from
user-defined symbols, because they require a special treatment from
the kernel.
\begin{code}
data Symbol =
    Builtin    B.BuiltinSymbol
  | UserSymbol SymbolId SymbolName ContextName

type SymbolId    = Int
type SymbolName  = String
type ContextName = String
\end{code}
A variable of type \inline{Symbol} is immutable; this it cannot
encompass the mutable nature of symbols in \emph{Kalkulu}. All mutable
informations attached to a particular symbol (its attributes, its
values, etc.) are defined elsewhere in the kernel. A variable of type
\inline{Symbol} serves as a mere identifier.

Here, \inline{B.BuiltinSymbol} is an enumeration type
listing all the built-in symbols.
\begin{spec}
-- defined in Kalkulu/Builtin.hs
data BuiltinSymbol =
  Plus | Power | Times -- and many more builtin symbols
  deriving(Show)
\end{spec}
The \inline{SymbolId} argument of the constructor \inline{UserSymbol}
serves two different purposes. On the one hand, it enables fast
access to the contents of a symbol. On the other hand, it allows to
distinguish homonym symbols; this situation can occur \emph{e.g.} in
the following snippet:
\begin{verbatim}
In[1] := a := b
In[2] := Remove[b]
In[3] := a === b
Out[3] = False
\end{verbatim}
By looking at the symbol identifying number only, we can instantiate
the \inline{Eq} typeclass efficiently.
\begin{code}
instance Eq Symbol where
  (Builtin x) == (Builtin y)               = x == y
  (UserSymbol x _ _) == (UserSymbol y _ _) = x == y
  _ == _                                   = False
\end{code}
\inline{Symbol}s are ordered by their name, and, in case of a tie, by
their context names. We can use the \inline{Monoid} structure of
\inline{Ordering} to express this in a concise way.
\begin{code}
instance Ord Symbol where
  compare x y  = (compare (getSymbolName x) (getSymbolName y))
                 <> (compare (getContextName x) (getContextName y))
    where getContextName (UserSymbol _ _ s) = s
          getContextName _                  = "System`"
          getSymbolName  (UserSymbol _ s _) = s
          getSymbolName  (Builtin s)        = show s
\end{code}
\end{document}
