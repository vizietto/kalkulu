\documentclass[main.tex]{subfiles}

\begin{document}
\section{Overview of the grammar}

The \emph{Kalkulu} grammar is simultaneously very simple and very
complicated. Simple, because it is an operator-precedence
grammar. Complicated, rather than difficult, because it has a huge
number of operators, many of which have special features. Here is a
list of some subtleties:
\begin{itemize}
\item Parentheses are used to modify the order of the precedence
  rules (\emph{e.g} the expression \verb?a*(b+c)? is parsed
  \verb?Times[a, Plus[b, c]]}?. They are not used for function call.
  Instead, we use square brackets\footnote{Strictly speaking, there
    is no such a thing as ``function calls'' in \emph{Kalkulu},
    though some expressions like \texttt{f[x]} look like function
    calls.}.
\item The three main associativity types for infix operators are
  \emph{left} associativity, \emph{right} associativity and
  \emph{flat} associativity. The table~\ref{tab:associativity_types}
  shows how a string \verb?"a op b op c"? is parsed, for an operator
  \verb?op?.
  \begin{table}[!h]
    \centering
    \begin{tabular}{c|c}
      Associativity type of \verb?op? & Parsed expression \\ \hline
      left associative  & \verb?Operator[Operator[a, b], c]? \\
      right associative & \verb?Operator[a, Operator[b, c]]? \\
      flat associative  & \verb?Operator[a, b, c]?
    \end{tabular}
    \caption{How the expression \texttt{a op b op c} is parsed}
    \label{tab:associativity_types}
  \end{table}
\item Multiplication can be implicit (as in mathematics): the strings
  \verb?"2a"? and \verb?"2 a"? are parsed as
  \verb?Times[2, a]?. However, the absence of space between two
  lexemes is not always considered as an implicit multiplication:
  \verb?"a_"? and \verb?"a _"? are respectively parsed
  \verb?Pattern[a, Blank[]]? and \verb?Times[a, Blank[]]?.
\item In sequence of expressions (separated by commas), an implicit
  \verb?Null? symbol appears in places where no expression is given
  near a comma. The input \verb?{,}? is parsed as
  \verb?List[Null, Null]? (however \verb?{}? is the empty list
  \verb?List[]?).

  Some infix operators may also enjoy this property, \emph{e.g} the
  operator \verb?;? (associated to \verb?CompoundExpression?). The
  input \verb?a;b;? is parsed as
  \verb?CompoundExpression[a, b, Null]?. (Note that the comma is not
  considered to be an operator).  
\item The end of line character indicates the end of an expression.
  For example, \verb?"2\na"? is a sequence of two expressions
  (\verb?2? and \verb?a?). However, this holds true if and only if
  the string before the end of line character is a well-formed
  expression. If not, \verb?'\n'? is treated as white space. For
  example, \verb?"f[2\na]"? is parsed as a single expression
  \verb?f[Times[2, a]]?. Actually, no backtracking is needed to
  determine the behaviour of \verb?'\n'? (here the white space was
  ignored because \verb?2\na? is a subexpression inside the composite
  expression \verb?f[2\na]?).
\end{itemize}

\end{document}
