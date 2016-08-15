\documentclass[main.tex]{subfiles}

\begin{document}
\chapter{Parsing}
The chapter describes the module \verb?Kalkulu.Parser?, whose
header is
\begin{code}
module Kalkulu.Parser where

import Control.Monad
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec hiding (space)

import qualified Kalkulu.Builtin as B
\end{code}

\section{Overview of the grammar}
\label{parser:sec:overview}

The \emph{Kalkulu} grammar is simultaneously very simple and very
complicated. Simple, because it is an operator-precedence
grammar. Complicated, rather than difficult, because it has a huge
number of operators, many of which have special features. Here is a
list of some subtleties:
\begin{itemize}
\item The purpose of parentheses is to modify the order of the
  precedence rules (\emph{e.g} the expression \verb?a*(b+c)? is
  parsed \verb?Times[a, Plus[b, c]]}?. They are not used for function
  calls.  Instead, we use square brackets\footnote{Strictly speaking,
    there is no such a thing as ``function calls'' in \emph{Kalkulu},
    though some expressions like \texttt{f[x]} look like function
    calls.}.
\item The three main associativity types for infix operators are
  \emph{left} associativity, \emph{right} associativity and
  \emph{flat} associativity. The
  table~\ref{parser:tab:associativity_types} shows how a string
  \verb?"a op b op c"? is parsed, for a given operator \verb?op?.
  \begin{table}[!h]
    \centering
    \begin{tabular}{c|c}
      Associativity type of \verb?op? & Parsed expression \\ \hline
      left associative  & \verb?Operator[Operator[a, b], c]? \\
      right associative & \verb?Operator[a, Operator[b, c]]? \\
      flat associative  & \verb?Operator[a, b, c]?
    \end{tabular}
    \caption{How the expression \texttt{a op b op c} is parsed}
    \label{parser:tab:associativity_types}
  \end{table}
\item Multiplication can be implicit (as in mathematics): the strings
  \verb?"2a"? and \verb?"2 a"? are parsed into
  \verb?Times[2, a]?. However, the absence of space is not always
  considered as an implicit multiplication: \verb?"a_"? and
  \verb?"a _"? are respectively parsed \verb?Pattern[a, Blank[]]? and
  \verb?Times[a, Blank[]]?.
\item In sequences of expressions (separated by commas), an implicit
  \verb?Null? symbol appears in places where no expression is given
  near a comma. The input \verb?{,}? is parsed as
  \verb?List[Null, Null]? (however \verb?{}? is the empty list
  \verb?List[]?).

  Some infix operators may also enjoy a similar property, \emph{e.g}
  the operator \verb?;? (associated to
  \verb?CompoundExpression?). The input \verb?a;b;? is parsed as
  \verb?CompoundExpression[a, b, Null]?. (Note that the comma is not
  considered to be an operator).
\item The end of line character indicates the end of an expression.
  For example, \verb?"2\na"? is a sequence of two expressions
  (\verb?2? and \verb?a?). However, this holds true if and only if
  the string before the end of line character is a well-formed
  expression. If not, \verb?'\n'? is treated as white space. For
  example, \verb?"f[2\na]"? is parsed as the single expression
  \verb?f[Times[2, a]]?. Actually, no backtracking is needed to
  determine the behaviour of \verb?'\n'? (here the white space was
  ignored because \verb?2\na? is a subexpression inside the composite
  expression \verb?f[2\na]?).
\end{itemize}

\section{Vocabulary and considerations}

A minimal term in the \emph{Kalkulu} operator-precedence grammar is
called a \emph{simple expression}. Simple expressions should not be
confused with atoms. Apart from simple expressions, there are
\emph{operators}, which combine simple expressions to form general
expressions. The table~\ref{parser:tab:simple_expressions} lists all
simple expressions.
\begin{table}[!h]
  \centering
  \begin{tabular}{|c|c|c|p{6.8cm}|}
    \hline
    & \textbf{Type} & \textbf{Example} & \textbf{Parsed expression}
    \\ \hline
    (SE1)  & Number   & \verb?123? & \verb?123? \\ \hline
    (SE2)  & String   & \verb?"abc"? & \verb?"abc"? \\ \hline
    (SE3a) &          & \verb?_?     & \verb?Blank[]? \\
    (SE3b) &          & \verb?_s? & \verb?Blank[s]? \\
    (SE3c) &          & \verb?_.?    & \verb?Optional[Blank[]]? \\
    (SE3d) & Blanks   & \verb?__?    & \verb?BlankSequence[]? \\
    (SE3e) &          & \verb?__s? & \verb?BlankSequence[s]? \\
    (SE3f) &          & \verb?___? & \verb?BlankNullSequence[]? \\
    (SE3g) &          & \verb?___s? & \verb?BlankNullSequence[s]? \\ \hline
    (SE4a) & Symbol   & \verb?s? & \verb?s? \\ \hline
    (SE4b) &          & \verb?s_? & \verb?Pattern[s, Blank[]]? \\
    (SE4c) &          & \verb?s1_s2? & \verb?Pattern[s1, Blank[s2]]? \\
    (SE4d) &          & \verb?s_.? & \verb?Optional[Pattern[s, Blank[]]]? \\
    (SE4e) & Patterns &  \verb?s__? & \verb?Pattern[s, BlankSequence[]]? \\
    (SE4f) & &\verb?s1__s2? & \verb?Pattern[s1, BlankSequence[s2]]? \\
    (SE4g) & &\verb?s___? & \verb?Pattern[s, BlankNullSequence[]]? \\
    (SE4h) & & \verb?s1___s2? & \verb?Pattern[s1, BlankNullSequence[s2]]? \\
    \hline
    (SE4i) & \multirow{2}{*}{Message}
                      & \verb?s1::s2? & \verb?MessageName[s1, "s2"]? \\
    (SE4j) &         & \verb?s1::s2::s3?
                      & \verb?MessageName[s1, "s2", "s3"]? \\ \hline
    (SE5a) &         & \verb?#? & \verb?Slot[1]? \\
    (SE5b) &         & \verb?#3? & \verb?Slot[3]? \\
    (SE5c) & Slots   & \verb?#abc? & \verb?Slot["abc"]? \\
    (SE5d) &         & \verb?##? & \verb?SlotSequence[1]? \\
    (SE5e) &         & \verb?##3? & \verb?SlotSequence[3]? \\ \hline
    (SE6a) &         & \verb?%? & \verb?Out[]? \\
    (SE6b) & Out     & \verb?%%%%? & \verb?Out[-4]? \\
    (SE6c) &         & \verb?%5? & \verb?Out[5]? \\ \hline
    (SE7) & Paren.   & \verb?(expr)? & \verb?expr? \\ \hline
    (SE8) & List     & \verb?{expr, ...}? & \verb?List[expr, ..]? \\ \hline
  \end{tabular}
  \caption{Simple expressions}
  \label{parser:tab:simple_expressions}
\end{table}

Displaying (the inverse problem of parsing) requires also to know the
precedences of operators. However, the notions of precedence for
displaying or parsing expressions may not
coincide. \emph{Mathematica} provides a function
\verb?Precedence?. Under this function, all boolean operators have
the same precedence (equal to \verb?215.?). We can easily guess that
this precedence regards displaying only. When we read
\verb?a || b && c?, we perfectly know that \verb?&&? is computed
before \verb?||?, because we are used to. Putting extra parentheses
around \verb?b && c? would be unwieldy. The moral is that we should
not rely too much on \verb?Precedence? in this chapter.

Here are some motivations on how to decide what is an operator,
a simple expression, etc. First, let us call \emph{lexeme} an
expression (possibly) followed by trailing whitespace.
\begin{itemize}
\item An operator combines lexemes to form complex expressions,
  but is not itself an expression. On this basis,
  \verb?_?, \verb?__? and \verb?___? are not treated as operators.
\item An expression which cannot be broken down into several
  lexemes should be a simple expression. Equivalently, if you have
  an expression, inside which you cannot add whitespace without
  modifying its meaning, then this expression is a simple expression.
  This criterium is satisfied by \verb?s1_s2?, as we saw in the
  section~\ref{parser:sec:overview}. This holds also true for
  \verb?s1::s2?. Indeed, if we add white space, for example as in
  \verb?s1 ::s2?, then we provoke a parser failure. This also
  shows that \verb?::? must not be treated as an operator (recall
  that an operator acts on lexemes).
\item The last condition is not necessary. It is not satisfied by
  compound expressions of type (SE7), (SE8).
\end{itemize}

\section{Simple expression parser}
Parsing in \emph{Kalkulu} is an impure process. To parse a symbol
\verb?symb?, one needs to know the current context, and this
information is only available at run time. Even if knew it, it is
also necessary to put new symbols in the symbol table.

For the moment we neglect all side-effects and use the following
type.
\begin{code}
type Name = String

data Expr =
    Number               Integer
  | String               String
  | Symbol               Name
  | Builtin              B.BuiltinSymbol
  | Cmp                  Expr [Expr]
    deriving Show 
\end{code}
The \emph{name} field of a \inline{Symbol} contains its name,
\emph{as it is read}, without further interpretation: it could be
\verb?"a"?, \verb?"Global`a"? or even \verb?"`a"?. The constructor
\inline{Cmp} is meant for composite expression, the first parameter
is the head, and the second is the argument list. Also, for the
moment, only integers are implemented in \emph{Kalkulu}.

\subsection{White space}
By \emph{white space}, we mean everything meaningless (included
comments). An end of line character can be meaningful depending on
the context (see discussion in
section~\ref{parser:sec:overview}). The argument of the following
never failing parser indicates whether or not to ignore end of lines.
\begin{code}
whitespace :: Bool -> Parser ()
whitespace ignoreEOL = skipMany $ (space <|> comment)
  where space = void $ satisfy isSpace'
        isSpace' c = isSpace c && (ignoreEOL || c /= '\n')
\end{code}
Comments are delimited by \verb?(*? and \verb?*)? and can be nested
arbitrarily deep.
\begin{code}
comment :: Parser ()
comment = do void $ try $ string "(*"
             skipMany $ (void (noneOf "(*") <|> star <|> par <|> comment)
             void $ string "*)"
             where star = try $ void $ char '*' >> notFollowedBy (char ')')
                   par  = try $ void $ char '(' >> notFollowedBy (char '*')
\end{code}
Except at the beginning of the main parser (to skip the leading white
space), one should not use the \inline{whitespace} parser very
often. Instead, the following function proves to be useful.
\begin{code}
lexeme :: Parser a -> Bool -> Parser a
lexeme p ignoreEOL = do x <- p
                        whitespace ignoreEOL
                        return x
\end{code}
It is insightful to see \inline{lexeme} as a combinator which maps a
parser to a \emph{lexemized} parser. In this order, we define an
alias type
\begin{code}
type LexParser a = Bool -> Parser a
\end{code}
The \inline{lexeme} function has the type
\begin{spec}
lexeme :: Parser a -> LexParser a
\end{spec}
It is tempting to see \inline{lexeme} as a kind of \inline{return}
function, embedding ordinary parsers in the class of lexemized
parsers.

\subsection{Numbers}
For the moment, only integers are implemented in \emph{Kalkulu}.
\textbf{TODO:} \verb?base^^digits?, floats, etc.
\begin{code}
natural :: Parser Expr
natural = many1 digit >>= (return . Number . read)
\end{code}
Notice that a number begins with a digit or a dot \verb?'.'?, in
particular it has no sign (\verb?'+'? or \verb?'-'?). Instead,
\verb?'+'? and \verb?'-'? are unary operators, dealt in
subsubsection~\textbf{TODO}. Thanks to that, a simple expression is
(almost) totally distinguishable from an operator by looking at the
first character\footnote{The only exception is \texttt{Dot}
  (\texttt{'.'}). We settle this issue by asking the dot operator not
  to be followed by a digit, see
  section~\ref{parser:sec:listops}.}. This property is important for
fast parsing, because a simple expression can be followed by either a
simple expression or an operator (an outcome of implicit
multiplication). Also, unary operator \verb?'-'?  is smart enough to
parse \verb?-2? as an atom rather than \verb?Times[-1, 2]?.

\subsection{Strings}
A \verb?String? is enclosed by quotes \verb?'\"'?. The escape
characters \verb?'\n'?, \verb?'\t'?, \verb?'\\'? and \verb?'\"'? are
allowed. As the combinator \inline{string} already exists in
\verb?Parsec?, we call our own combinator \inline{string'}. It
parses simple expressions of type (SE2).
\begin{code}
string' :: Parser Expr
string' = liftM String $ between (char '"') (char '"')
                                 (many (noneOf "\"\\" <|> escapeChar))
              
escapeChar :: Parser Char
escapeChar = do
  _ <- char '\\'
  e <- oneOf "nt\"\\" <?> "escape character"
  return $ case e of 'n'  -> '\n'
                     't'  -> '\t'
                     _    -> e
\end{code}

\subsection{Symbols}
A symbol begins with a letter, or the character \verb?'$'?, then it
may contain any alphanumeric character, or \verb?'$'?.  Additionally,
a symbol may contain the character \verb?'`'?  (backquote) to
separate its proper name from its context name(s). Contexts can be
nested in one another. The full name of a symbol involves a sequence
of context names \verb?context1`context2`...`name?. A symbol
identifier can also begin with \verb?'`'?. In this case, it means
that the symbol shoud be searched within the current context. Apart
from this case, a context name must not be empty. This implies that
cannot contain two consecutive backquotes, nor can it end with a
\verb?'`'?.
\begin{code}
backquote :: Parser Char
backquote = char '`' >> (lookAhead letter <?> "non-empty context name")
                     >> return '`'

identFirst :: Parser Char
identFirst = letter <|> backquote

identLetter :: Parser Char
identLetter = alphaNum <|> backquote

identifier :: Parser String
identifier = (:) <$> identFirst <*> many identLetter
\end{code}
Finally, we can parse simple expressions of type (SE4a).
\begin{code}
symbol :: Parser Expr
symbol = Symbol <$> identifier
\end{code}

\subsection{Blanks, Patterns and Messages}
In this subsection, we first parse expressions of type (SE3).
Because of the similarity with the expressions (SE4b) -- (SE4h), this
part will be used to parse patterns as well.  The first symbol (left
of \verb?_? in (SE4b) -- (SE4h)) is called the \emph{pattern} symbol.

We will define a function
\begin{spec}
blank :: Maybe Expr -> Parser Expr
\end{spec}
which takes as an argument a previously parsed pattern symbol, or
nothing in case of an anonymous blank (type (SE3)), and parses the
whole expression. To parse an anonymous blank, simply use
\inline{blank Nothing}.
\begin{code}
blank :: Maybe Expr -> Parser Expr
blank p = char '_' >> (
  (try (string "__") >> (se3g <|> se3f))
  <|> (char '_' >> (se3e <|> se3d))
  <|> try se3c <|> se3b <|> se3a)
  where se3a = makeBlank B.Blank
        se3b = makeBlank' B.Blank
        se3c = do void $ char '.'
                  notFollowedBy (char '.')
                  return $ Cmp (Builtin B.Optional)
                               [makePattern $ Cmp (Builtin B.Blank) []]
        se3d = makeBlank  B.BlankSequence
        se3e = makeBlank' B.BlankSequence
        se3f = makeBlank  B.BlankNullSequence
        se3g = makeBlank' B.BlankNullSequence
        makePattern arg = case p of
          Nothing -> arg
          Just x  -> Cmp (Builtin B.Pattern) [x, arg]
        makeBlank b = return $ makePattern $ Cmp (Builtin b) []
        makeBlank' b = do
          s <- symbol
          return $ makePattern $ Cmp (Builtin b) [s]
\end{code}
Note that in the case (SE3c), the character \verb?'.'? cannot be
followed by another dot, because the expression \verb?_..? is parsed
as \verb?Repeated[Blank[]]? (here \verb?..? is as a postfix
operator). In order to correctly parse this expression, we need to
put a \inline{try} in front of this litigious case.

It remains to parse expressions of type (SE4i). The syntax
\verb?s1::s2? is peculiar: \verb?s2? is parsed as a string, though it
is not delimited by quotes. Moreover, \verb?s2? has the syntax of a
symbol identifier, without backquotes. This holds also for
expressions \verb?#abc? of type (SE5c) (except that the identifier
\verb?abc? can contain backquotes).

The following function \inline{vse4i} parses from
\verb?::s2(::s3)?, assuming that the symbol \verb?s1? has been
previously parsed and passed as an argument.
\begin{code}
se4ij :: Expr -> Parser Expr
se4ij s1 = do
  void $ try $ string "::"
  s2 <- identifier
  (do void $ try $ string "::"                                       -- SE4j
      s3 <- identWithoutBackquote
      return $ Cmp (Builtin B.MessageName) [s1, String s2, String s3])
   <|> (return $ Cmp (Builtin B.MessageName) [s1, String s2])        -- SE4i
  where identWithoutBackquote = (:) <$> letter <*> many alphaNum
\end{code}
Note the \inline{try} combinator in front of every occurence of
\verb?string "::"?. This is because \verb?::? should not be confused
with the infix operator \verb?':'? (representing \verb?Pattern?).
Now, we are equipped to parse simple expressions of type (SE4a) --
(SE4j).
\begin{code}
se4 :: Parser Expr
se4 = symbol >>= \s ->
  (notFollowedBy ((void $ char '_') <|> (void $ string "::")) >> return s)
  <|> (blank $ Just s)
  <|> se4ij s
\end{code}
In \emph{Mathematica}, the syntax is slightly more permissive,
expressions of type \verb?simpleExpr::s? are allowed.  This makes the
definition of simple expressions left-recursive.  Moreover, this
complication only enables to write non sensical expressions. There is
no restriction in \emph{Kalkulu} because those expressions can still
be written in \verb?FullForm?.
\subsection{Slots and Outs}
The following parses expressions of type (SE5) and (SE6).
\begin{code}
slot :: Parser Expr
slot = char '#' >> (
  (do i <- natural
      return $ Cmp (Builtin B.Slot) [i])                             -- SE5b
  <|> (do s <- identifier
          return $ Cmp (Builtin B.Slot) [String s])                  -- SE5c
  <|> (char '#' >> ((do i <- natural                                 -- SE5e
                        return $ Cmp (Builtin B.SlotSequence) [i])
                    <|> (return $ Cmp (Builtin B.SlotSequence) []))) -- SE5d
  <|> (return $ Cmp (Builtin B.Slot) []))                            -- SE5a

out :: Parser Expr
out = char '%' >> (
  (do i <- natural
      return $ Cmp (Builtin B.Out) [i])                              -- SE6c
  <|> (do i <- toInteger <$> length <$> many (char '%')
          return $ case i of
             0 -> Cmp (Builtin B.Out) []                             -- SE6a
             _ -> Cmp (Builtin B.Out) [Number (-i-1)]))              -- SE6b
\end{code}
\subsection{Bracketed expressions}
The following parses parenthesized expressions (type (SE7)). A
general pattern is that when we deal with delimited expressions, the
opening token (here \verb?'('?)  has to be lexemized. Spaces
(including end of lines) are ignored \emph{before}, \emph{inside} and
\emph{after} the parenthesized expression (\inline{expr True} is
already lexemized, see subsection~\ref{parser:sub:algorithm}, it
would be redundant to replace it with \inline{lexeme (expr True)
 True}).
\begin{code}
parenthesizedExpr :: Parser Expr
parenthesizedExpr = between (lexeme (char '(') True) (char ')') (expr True)
\end{code}
Among delimited expressions, expressions of type (SE7) are exceptions
because they can contain only one subexpression (both \verb?()?  and
\verb?(e1, e2)? provoke a failure).

Next, we write a general function to parse a succession of comma
separated expressions.  We take care of implicit \verb?Null? symbols,
which slightly obfuscates the code (we cannot use a simple
\inline{sepBy} because we do not want an empty list \verb?{}? to be
parsed as \verb?List[Null]?).
\begin{code}
bracketed :: Parser () -> Parser () -> Parser [Expr]
bracketed opening closing = do
  lexeme opening True
  first <- firstArg
  rest  <- many $ commaArg
  closing
  return $ case (first, rest) of
    (Nothing, []) -> []
    (Nothing, xs) -> (Builtin B.Null) : xs
    (Just x , xs) -> x : xs
  where firstArg = optionMaybe (expr True)
        arg = lexeme (expr True <|> implicitNull) True
        commaArg = (lexeme (char ',') True) >> arg
        implicitNull = return (Builtin B.Null)
\end{code}
We can now parse lists in form \verb?{...}?. In the future, we can
use \inline{bracketed} to parse more bracketed expressions, like
\verb?Association?s, etc.
\begin{code}
list :: Parser Expr
list = listify <$> bracketed (void $ char '{') (void $ char '}')
  where listify x = Cmp (Builtin B.List) x
\end{code}
\subsection{Conclusion}
Thanks to the preceding subsections, we are able to create a parser
of simple expressions.
\begin{code}
simpleExpr :: Parser Expr
simpleExpr = natural <|> string' <|> blank Nothing
             <|> se4 <|> slot <|> out
             <|> parenthesizedExpr <|> list
\end{code}
No \inline{try} combinator is needed because each of the eight
categories of simple expressions begins with a distinctive letter:
\begin{itemize}
\item numbers begin with \verb?'.'? or a digit,
\item strings begin with \verb?'"'?,
\item blanks begin with \verb?'_'?,
\item expressions of type (SE4) begin with \verb?'$'?, \verb?'`'? or
  a letter,
\item slots begin with \verb?'#'?,
\item outs begin with \verb?'%'?,
\item parenthesized expressions begin with \verb?'('?,
\item lists begin with \verb?'{'?.
\end{itemize}

\section{Operator precedence parser}
\subsection{Vocabulary}
\label{parser:subsec:vocabulary}
First, let us introduce some vocabulary. We call a \emph{precedence
  level} a collection of operators of equal precedence (an equivalence
class under the relation ``having the same precedence as''). Common
examples are $\{\verb?+?, \verb?-?\}$ or $\{\verb?===?, \verb?=!=?\}$
(many of those classes are singletons, like $\{\verb?&&?\}$, etc.)
The \emph{principal precedence level} in an expression is the ``last
executed'' class of operators. For example, in \verb?a+b*c-d?, the
principal precedence level is $\{\verb?+?, \verb?-?\}$ (addition and
subtraction have lower precedence than multiplication). Precedence
levels are totally ordered, of course by precedence... The precedence
level $\{\verb?+?, \verb?-?\}$ (addition and subtraction) is for
example lower than $\{\verb?^?\}$ (exponentiation). If $e$ is a
precedence level, we call $e^+$ its successor
\[
e^+ := \min \{e' \mid e' > e\}.
\]
Often, only one operator in a principal level
appears, in this case, we call it the \emph{principal operator}.  In
order to avoid cumbersome notations, we shall avoid the notion of
principal precedence level, and present only examples with a
principal operator.

The principal operator divide the whole expression into
subexpressions. We call \emph{expression tail} what is found right of
the first subexpression, see Table~\ref{parser:tab:principal_op} for
some examples.

\begin{table}[!h]
  \centering
  \begin{tabular}{c|c|c|c|c}
    \textbf{Case} & \textbf{Expression} & \textbf{Prin. op.} &
    \textbf{Subexpressions} & \textbf{Expression tail} \\ \hline
    1 & \verb?a+b*c+d? & \verb?+? & \verb?a, b*c, d? & \verb?+b*c+d? \\
    2 & \verb?a[b,c]? & \verb?[b,c]? & \verb?a? & \verb?[b,c]? \\
    3 & \verb?!a? & \verb?!? & \verb?a? & $\emptyset$ \\
    4 & \verb?a+!b? & \verb?+? & \verb?a, !b? & \verb?+!b? \\
    5 & \verb?a&*b + c? & \verb?+? & \verb?a&*b, c? & \verb?+c? \\
    6 & \verb?a&++? & \verb?++? & \verb?a&? & \verb?++? \\
    7 & \verb?++!a? & \verb?++? & \verb?!a? & $\emptyset$
  \end{tabular}
  \caption{Principal operators and expression tails}
  \label{parser:tab:principal_op}
\end{table}

Note that (case 2 of Table~\ref{parser:tab:principal_op}) we consider
the argument list in a composite expression as a postfix operator.
When the principal operator is a prefix operator (cases 3 and 7),
the expression has no tail. One important remark is that the
principal operator is not necessarily the one of least precedence.
Four situations can show up where this holds wrong:
\begin{itemize}
\item the principal operator \verb?op? is infix and the first
  subexpression contains a postfix operator of precedence lower than
  \verb?op? (case 5),
\item the principal operator \verb?op? is infix and the last
  subexpression contains a prefix operator of lower precedence than
  \verb?op? (case 4),
\item a postfix operator \verb?op? is applied to a subexpression
  whose principal operator is a postfix operator of precedence lower
  than \verb?op? (case 6),
\item a prefix operator \verb?op? is applied to a subexpression whose
  principal operator is a prefix operator of precedence lower than
  \verb?op? (case 7).
\end{itemize}
Last, one should see the expression tail as a function, which maps
a missing first subexpression to the whole expression, for example
in case 1:
\[
\verb?a? \mapsto \verb?Plus[a, Times[b, c], d]?.
\]
\subsection{Algorithm}
\label{parser:sub:algorithm}

The strategy for parsing expressions is to make full use of parser
combinators. We start from \inline{simpleExpr} (the parser for simple
expressions) and generate new parsers which can recognize more and
more operators. This is not a straightforward task. Cases 4, 5 of
Table~\ref{parser:tab:principal_op} show that, to parse an addition,
one needs to already ``know'' the unary operators of lower
precedence...

Let $e$ be a precedence level. We call $\mathcal{L}_e$ the language
consisting of simple expressions, and expressions with principal
precedence level $\geq e$ such that no postfix operator of precedence
strictly lower than $e$ appears.

When $e = \{\verb?+?, \verb?-?\}$, then cases 1, 2, 4 and 7 of
Table~\ref{parser:tab:principal_op} belong to $\mathcal{L}_e$. Case 3
fails because the principal operator \verb?'!'? (\verb?Not?) has
precedence lower than $e$. Case 5 fails because the postfix operator
\verb?'&'? is involved, so does case 6. However, \verb?(a&)+b?
belongs to $\mathcal{L}_e$, because \verb?'&'? is hidden in
\verb?(a&)?, which is a simple expression (type (SE7) in
Table~\ref{parser:tab:simple_expressions}).

Note that, when $e$ is the lowest precedence class (\verb?CompoundExpression?), $\mathcal{L}_e$ consists of all possible expressions.
One can infer $\mathcal{L}_e$ from $\mathcal{L}_{e^+}$. First, when
$e$ is infix, then
\begin{equation}
todo
\end{equation}

\subsection{Implementation}
A precedence level (or operator list) is represented by the data type
below. We impose some constraints: it is impossible to mix several
associativity types within precedence level. Also, unary operators
are unique in their precedence level. All constructors starting from
{\bf TODO} are meant to cope with special cases.
\begin{code}
data PrecedenceLevel =
    Prefix                       (LexParser (Expr -> Expr))
  | Postfix                      (LexParser (Expr -> Expr))
  | forall a. InfixOp a => Infix [LexParser a]
\end{code}
However, several infix operators can share the same precedence.
The typeclass \inline{InfixOp} determines how the different
subexpressions are combined.
\begin{code}
class InfixOp a where
  makeExpression :: Expr -> [(a, Expr)] -> Expr
\end{code}
Using vocabulary from subsection~\ref{parser:subsec:vocabulary},
the first argument of \inline{makeExpression} represents the
first subexpression, and the second represents the expression tail.

We need to update simultaneously parsers for terms, expression tails,
and prefixed expressions. We use the type alias \inline{TrioParser}
for a triplet of parsers.
\begin{code}
type TrioParser  = (LexParser Expr, LexParser (Expr -> Expr), LexParser Expr)
\end{code}
The following functions takes a triplet
\verb?(term, tailExpr, prefixed)?, a precedence level, and returns an
enhanced parser, able to parse operators from the precedence level.
\begin{spec}
makeParser :: TrioParser -> PrecedenceLevel -> TrioParser
\end{spec}
The table of operators \inline{opTable :: [PrecedenceLevel]}
(documented in section~\ref{parser:sec:listops}) contains all
operators in order of decreasing precedence.

The final parser \inline{expr} is built by making repeated use of
\inline{makeParser}.
\begin{code}
expr :: LexParser Expr
expr = let first (x, _, _) = x in first finalTrio

anyPrefixed :: LexParser Expr
anyPrefixed = let third (_, _, x) = x in third finalTrio

finalTrio :: TrioParser
finalTrio = foldl makeParser initialTrio opTable
  where initialTrio = (lexeme simpleExpr, const mzero, const mzero)
\end{code}

\begin{code}
makeParser :: TrioParser -> PrecedenceLevel -> TrioParser
makeParser (term, tailExpr, prefixed) (Infix ops) =
  (term', tailExpr', prefixed)
  where
  term' ignoreEOL = do x <- term ignoreEOL
                       (do y <- tailInfix ignoreEOL
                           return $ y x) <|> return x
  tailInfix ignoreEOL = do
    xs <- many1 $ (,) <$> infixOp <*> (term ignoreEOL<|> anyPrefixed ignoreEOL)
    return $ \x -> makeExpression x xs
  tailExpr' ignoreEOL = tailExpr ignoreEOL <|> tailInfix ignoreEOL
  infixOp = choice [try (op True) | op <- ops]

makeParser (term, tailExpr, prefixed) (Postfix op) =
  (term', tailExpr', prefixed)
  where
  term' ignoreEOL = do x <- term ignoreEOL
                       ys <- many $ tailExpr' ignoreEOL
                       return $ foldl (flip ($)) x ys
  tailExpr' ignoreEOL = tailExpr ignoreEOL <|> op ignoreEOL

makeParser (term, tailExpr, prefixed) (Prefix op) =
  (term', tailExpr, prefixed')
  where
  term' ignoreEOL = (term ignoreEOL) <|> newPrefixed ignoreEOL
  newPrefixed ignoreEOL = do prefixOp <- (op True)
                             x <- term ignoreEOL <|> anyPrefixed ignoreEOL
                             return $ prefixOp x
  prefixed' ignoreEOL = prefixed ignoreEOL <|> newPrefixed ignoreEOL
\end{code}

\begin{code}
data InfixF = Composition | StringJoin | NonCommutativeMultiply
  | Dot | SameQ | UnsameQ | And | Or | Alternative | StringExpression
  deriving Eq

instance InfixOp InfixF where
  makeExpression h tl = foldl (\e (op, es) -> Cmp (Builtin $ toSymbol op) (e:es)) h (helper tl)
    where
    helper [] = []
    helper ((op, e):xs) = case (helper xs) of
      []             -> [(op, [e])]
      l@((op',es):t) -> if op == op' then (op,e:es):t else (op,[e]):l
    toSymbol Composition = B.Composition
    toSymbol StringJoin = B.StringJoin
    toSymbol NonCommutativeMultiply = B.NonCommutativeMultiply
    toSymbol Dot = B.Dot
    toSymbol SameQ = B.SameQ
    toSymbol UnsameQ = B.UnsameQ
    toSymbol And = B.And
    toSymbol Or = B.Or
    toSymbol Alternative = B.Alternative
    toSymbol StringExpression = B.StringExpression
\end{code}

\begin{code}
prefix :: String -> B.BuiltinSymbol -> PrecedenceLevel
prefix opName symb = Prefix (\ignoreEOL ->
                               lexeme (string opName) ignoreEOL >>
                               return (\h -> Cmp (Builtin symb) [h]))

postfix :: String -> B.BuiltinSymbol -> PrecedenceLevel
postfix opName symb = Postfix (\ignoreEOL ->
                                 lexeme (string opName) ignoreEOL >>
                                 return (\h -> Cmp (Builtin symb) [h]))

infix' :: InfixOp a => [(String, a)] -> PrecedenceLevel
infix' xs = Infix [lexeme (string opName >> return op) | (opName, op) <- xs]

opTable :: [PrecedenceLevel]
opTable = [
  prefix "<<" B.Get,
  postfix "++" B.Increment,
  postfix "--" B.Decrement,
  prefix "++" B.PreIncrement,
  prefix "--" B.PreDecrement,
  postfix "!!" B.Factorial2,
  postfix "!" B.Factorial,
  infix' [("<>", StringJoin)],
  infix' [("**", NonCommutativeMultiply)],
  Infix [lexeme (char '.' >> notFollowedBy digit >> return Dot)],
  prefix "!" B.Not,
  postfix "..." B.RepeatedNull,
  postfix ".." B.Repeated,
  postfix "&" B.Function]
\end{code}
\end{document}
