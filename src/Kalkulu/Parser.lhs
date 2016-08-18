\documentclass[main.tex]{subfiles}
\def\hideFromLaTeX#{}

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

\hideFromLaTeX{
\begin{code}
data UnaryMultiplication = UPlus | UMinus
data Multiplication = Times | Divide
\end{code}
}

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
  parsed \verb?Times[a, Plus[b, c]]?). They are not used for function
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
subsubsection~\ref{parser:subsub:multiplication}. Thanks to that, a
simple expression is (almost) totally distinguishable from an
operator by looking at the first character\footnote{The only
  exception is \texttt{Dot} (\texttt{'.'}). We settle this issue by
  asking the dot operator not to be followed by a digit, see
  subsection~\ref{parser:subsec:listops}.}. This property is
important for fast parsing, because a simple expression can be
followed by either a simple expression or an operator (an outcome of
implicit multiplication). Also, unary operator \verb?'-'?  is smart
enough to parse \verb?-2? as an atom rather than \verb?Times[-1, 2]?.

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
\label{parser:subsec:bracketed_expressions}
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
principal precedence level, and present mainly examples with a
principal operator.

The principal operator divides the whole expression into
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
One should see the expression tail as a function, which maps
a missing first subexpression to the whole expression, for example
in case 1:
\[
\verb?a? \mapsto \verb?Plus[a, Times[b, c], d]?.
\]

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

Note that, when $e$ is the lowest precedence class
(\verb?CompoundExpression?), $\mathcal{L}_e$ consists of all possible
expressions.  One can infer $\mathcal{L}_e$ from
$\mathcal{L}_{e^+}$. First, when $e$ is infix, then
\begin{verbatim}
<term>_e     := <term>_e+ (<op>_e (<term>_e+ | <anyPrefixed>))*
<tailExpr>_e := <tailExpr>_e+ | (<op>_e (<term>_e+ | <anyPrefixed>))*
<prefixed>_e := <prefixed>_e+
\end{verbatim}
Here, \verb?<term>_e? denotes a generic element of $\mathcal{L}_e$,
and \verb?<op>_e? denotes any operator of the precedence level $e$.
Finally \verb?<anyPrefixed>? denotes a generic expression whose
principal operator is a prefix (even those of precedence lower than
$e$).  When $e$ is postfix, one has
\begin{verbatim}
<term>_e     := <term>_e+ (<tailExpr>_e)*
<tailExpr>_e := <tailExpr>_e+ | <op>_e
<prefixed>_e := <prefixed>_e+
\end{verbatim}
The intuition is the following. The expression \verb?a& + b?  does
not belong to $\mathcal{L}_{\{+,-\}}$ because of $\verb?&?$, but it
belongs to $\mathcal{L}_{\{\texttt{\&}\}}$. Thus, expressions in
$\mathcal{L}_{\{\texttt{\&}\}}$ are not only of the form
\verb?expr &?: one must have the right to add as many expression
tails as wanted: \verb?expr & * a * b? or even
\verb?expr & * a * b + c + d?. This is why we introduced
\verb?<tailExpr>_e?, which represents any expression tail for a
principal operator of precedence $\geq e$.  Finally,
\verb?<prefixed>_e? is needed because of the presence of
\verb?<anyPrefixed>? above.

When $e$ is prefix, one has
\begin{verbatim}
<term>_e     := <term>_e+ | <op>_e (<term>_e | <anyPrefixed>_e)
<tailExpr>_e := <tailExpr>_e+
<prefixed>_e := <prefixed>_e+ | <op>_e (<term>_e | <anyPrefixed>_e)
\end{verbatim}
\subsection{Implementation}
A precedence level (or operator list) is represented by the data type
below. We impose some constraints: it is impossible to mix several
associativity types within precedence level. Also, unary operators
are unique in their precedence level. All constructors starting from
\inline{CompoundExpression} are meant to cope with special cases.
\begin{code}
data PrecedenceLevel =
    Prefix                       (LexParser (Expr -> Expr))
  | Postfix                      (LexParser (Expr -> Expr))
  | forall a. InfixOp a => Infix [LexParser a]
  | CompoundExpression
  | Multiplication
  | PatternTest
\end{code}
As opposed to unary operators, several infix operators can share the
same precedence.  The typeclass \inline{InfixOp} determines how the
different subexpressions are combined.
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
(documented in subsection~\ref{parser:subsec:listops}) contains all
operators in order of decreasing precedence.

The final parser \inline{expr} is built by making repeated use of
\inline{makeParser}.
\begin{code}
finalTrio :: TrioParser
finalTrio = foldl makeParser initialTrio opTable
  where initialTrio = (lexeme simpleExpr, const mzero, const mzero)

expr :: LexParser Expr
expr = let first (x, _, _) = x in first finalTrio

anyPrefixed :: LexParser Expr
anyPrefixed = let third (_, _, x) = x in third finalTrio
\end{code}
We implement the algorithm described in
subsection~\ref{parser:sub:algorithm}.
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
  infixOp = choice [op True | op <- ops]
\end{code}

\begin{code}
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

\subsection{Special cases}
\subsubsection{Compound expressions}
The operator \verb?';'? (\verb?CompoundExpression?) is a particular
infix operator: its terms (except the first), can be implicitly
\verb?Null?. For example, \verb?a; ;b? is parsed
\verb?CompoundExpression[a, Null, b]?. As such, the expression
\verb?a;? is perfectly valid. Consequently, \verb?';'? shares some
properties with postfix operator.
\begin{code}
makeParser (term, tailExpr, prefixed) CompoundExpression =
  (term', tailExpr, prefixed)
  where
  term' ignoreEOL = do
    x  <- term ignoreEOL
    ys <- many $ tailExpr' ignoreEOL
    return $ foldl (flip ($)) x ys
  tailInfix ignoreEOL = do
    y <- many1 $ subexpr ignoreEOL
    return $ \x -> Cmp (Builtin B.CompoundExpression) (x:y)
  subexpr ignoreEOL = lexeme (char ';') ignoreEOL >>
    (term ignoreEOL <|> return (Builtin B.Null))
  tailExpr' ignoreEOL = tailExpr ignoreEOL <|> tailInfix ignoreEOL
\end{code}

\subsubsection{Multiplication}
\label{parser:subsub:multiplication}
This part is rather tricky for many reasons. First, there are four
operators related to multiplication: two infix operators \verb?'*'?
and \verb?'/'? (\verb?Times? and \verb?Divide?) and two prefix
operators \verb?'+'? and \verb?'-'? (the unary \verb?Plus? and
\verb?Minus?). It is the only time in \emph{Kalkulu} that prefix
and infix operators belong to the same precedence level.

An other danger is that \verb?Times? can be implicit: \verb?2a? is
the same as \verb?2*a?. Warning! \verb?2+a? is not the same as
\verb?2(+a)?, as you might guess. But this has to be carefully
verified, because the parser does not ``know'' addition yet (it has
a lower precedence).

We define the types
\begin{spec}
data UnaryMultiplication = UPlus | UMinus
data Multiplication = Times | Divide
\end{spec}
Below, \inline{pmTimes :: Parser ([UnaryMultiplication], Expr)} is a
parser for a term (factor) preceded by any number of \verb?Plus?  or
\verb?Minus?. An expression like \verb?-2? is parsed as an atom, but
this kind of simplification does not occur any more if we pile up
prefix operators: \verb?- -2? is parsed as \verb?Times[-1, -2]?.
The function \inline{factors} allows to transform a \inline{pmTerm}
into a series of factors.
\begin{code}
makeParser (term, tailExpr, prefixed) Multiplication =
  (term', tailExpr', prefixed')
  where
  unaryPlus  = try (lexeme (char '+' >> notFollowedBy (char '+')) True
                    >> return UPlus)
  unaryMinus = try (lexeme (char '-' >> notFollowedBy (char '-')) True
                    >> return UMinus)
  divide = try (lexeme (char '/' >> notFollowedBy (oneOf "/.;=")) True
                >> return Divide)
  times  = try $ (lexeme (char '*' >> notFollowedBy (char '=')) True
                   <|> (notFollowedBy $ oneOf "+-")) >> return Times
  pmTerm ignoreEOL = (,) <$> (many $ choice [unaryPlus, unaryMinus])
                         <*> (term ignoreEOL <|> anyPrefixed ignoreEOL)
  newPrefixed ignoreEOL = do
    pms <- (many1 $ choice [unaryPlus, unaryMinus])
    e <- (term ignoreEOL <|> anyPrefixed ignoreEOL)
    return $ addTimes $ factors (pms, e)
  prefixed' ignoreEOL = prefixed ignoreEOL <|> newPrefixed ignoreEOL
  infixOp = divide <|> times
  term' ignoreEOL = do x <- pmTerm ignoreEOL
                       (do y <- tailInfix ignoreEOL
                           return $ addTimes $ (factors x)++(tailFactors y))
                           <|> return (addTimes (factors x))
  tailInfix ignoreEOL = many1 $ (,) <$> infixOp <*> pmTerm ignoreEOL
  factors ([], x) = [x]
  factors ([UMinus], Number x) = [Number (-x)]
  factors (UMinus:xs, y) = (Number (-1)):factors (xs, y)
  factors (UPlus:xs, y) = [Cmp (Builtin B.Plus) [addTimes (factors (xs, y))]]
  addTimes [e] = e
  addTimes es = Cmp (Builtin B.Times) es
  applyInfix (Times,  x) = factors x
  applyInfix (Divide, x) =
    [Cmp (Builtin B.Power) [addTimes (factors x), Number (-1)]]
  tailFactors es = concat (map applyInfix es)
  tailExpr' ignoreEOL = tailExpr ignoreEOL <|> (do
    y <- tailInfix ignoreEOL
    return $ \h -> addTimes (h:tailFactors y))
\end{code}

\subsubsection{PatternTest}
The operator \verb|'?'| (\verb?PatternTest?) is the only non
associative in \emph{Kalkulu}. The expression \verb|a ? b ? c| is
illegal.
\begin{code}
makeParser (term, tailExpr, prefixed) PatternTest =
  (term', tailExpr', prefixed)
  where
  term' ignoreEOL = do x <- term ignoreEOL
                       (do y <- tailInfix ignoreEOL
                           return $ y x) <|> return x
  tailInfix ignoreEOL = do
    void $ lexeme (char '?') True
    y <- term ignoreEOL
    notFollowedBy (char '?')
    return $ \x -> Cmp (Builtin B.PatternTest) [x, y]
  tailExpr' ignoreEOL = tailExpr ignoreEOL <|> tailInfix ignoreEOL
\end{code}

\subsubsection{Span}
It is challenging to categorize \verb?;;?. Is it an operator?  A
standalone \verb?;;? makes perfect sense as an expression. As an
operator, it can be postfix, prefix, infix, ternary, and even
worse. The Table~\ref{parser:span} enumerates all the constructions
involving \verb?;;?.
\begin{table}[!h]
  \centering
  \begin{tabular}{c|c|c}
    Case & Input & Parsed expression \\ \hline
    1 & \verb?a;;b? & \verb?Span[a, b]? \\
    2 & \verb?a;;?  & \verb?Span[a, All]? \\
    3 & \verb?;;b? & \verb?Span[1, b]? \\
    4 & \verb?;;? & \verb?Span[1, All]? \\
    5 & \verb?a;;b;;step? & \verb?Span[a, b, step]? \\
    6 & \verb?a;;;;step? & \verb?Span[a, All, step]? \\
    7 & \verb?;;b;;step? & \verb?Span[1, b, step]? \\
    8 & \verb?;;;;step? & \verb?Span[1, All, step]?
  \end{tabular}
  \caption{Span}
  \label{parser:span}
\end{table}
Now, how should an expression like \verb?a;;b;;c;;;;? be parsed?
We read from the left and look greedily for the longest match of an
expression found in Table~\ref{parser:span} (this is exactly what
the local function \verb?extract? does):
\[
 \left(\verb?a;;b;;c?\right) \left(\verb?;;?\right)
\left(\verb?;;?\right).
\]
We consequently parse \verb?a;;b;c;;;;? as
\[
\verb?Times[Span[a, b, c], Span[1, All], Span[1, All]]?.  
\]
But multiplication has a higher precedence than \verb?Span?.
So, unfortunately, the different \verb?Span? factors will not
regroup themselves in a \verb?Times?. This is something the
following snippet has to do manually.
\begin{code}
-- TODO!
\end{code}

\subsection{Associativity}
\subsubsection{Flat associative infix operators}
Below, we find the list of flat associative operators in
\emph{Kalkulu}. As in Haskell, it is possible to transform a symbol
into an infix operator (by surrounding it with \verb?'~'?). In the
expression \verb?a ~symb~ b?, all of \verb?~symb~? is considered as
an operator, represented by \inline{Tilde "symb"}.
\begin{code}
data InfixF = Composition | StringJoin | NonCommutativeMultiply
  | Dot | SameQ | UnsameQ | And | Or | Alternative | StringExpression
  | Tilde Name
  deriving Eq
\end{code}
We already discussed flat associative operators,
see~\ref{parser:sec:overview}. It remains to be said that in some
cases, several flat associative operators can share the same
precedence.  The most important case is \verb?===? and \verb?=!=?,
operators associated to the symbols \verb?SameQ? and
\verb?UnsameQ?. The Table~\ref{parser:tab:flat_operators} shows what
happens in this case.
\begin{table}[!h]
  \centering
  \begin{tabular}{c|c}
    Input & Parsed expression \\ \hline
    \texttt{a === b === c =!= d =!= e} &
        \texttt{UnsameQ[SameQ[a, b, c], d, e]} \\
    \texttt{a =!= b =!= c === d === e =!= f} &
        \texttt{UnsameQ[SameQ[UnsameQ[a, b, c], d, e], f]}
  \end{tabular}
  \caption{Parsing flat associative operators of the same precedence}
  \label{parser:tab:flat_operators}
\end{table}
\begin{code}
instance InfixOp InfixF where
  makeExpression h tl = foldl (\e (op, es) -> Cmp (toExpr op) (e:es))
                              h (helper tl)
    where
    helper [] = []
    helper ((op, e):xs) = case (helper xs) of
      []             -> [(op, [e])]
      l@((op',es):t) -> if op == op' then (op,e:es):t else (op,[e]):l
    toExpr Composition = Builtin B.Composition
    toExpr StringJoin = Builtin B.StringJoin
    toExpr NonCommutativeMultiply = Builtin B.NonCommutativeMultiply
    toExpr Dot = Builtin B.Dot
    toExpr SameQ = Builtin B.SameQ
    toExpr UnsameQ = Builtin B.UnsameQ
    toExpr And = Builtin B.And
    toExpr Or = Builtin B.Or
    toExpr Alternative = Builtin B.Alternative
    toExpr StringExpression = Builtin B.StringExpression
    toExpr (Tilde x) = Symbol x
\end{code}
Let us see what happens in the second example of
Table~\ref{parser:tab:flat_operators}. The variable \verb?h? is
bound to \verb?a?, whereas \verb?tl? is bound to
\verb?[(=!=, b), (=!=, c), (===, d), (===, e), (=!=, [f])]?.
Then, applying \verb?tl? to the helper function
\begin{spec}
helper :: Eq a => [(a, b)] -> [(a, [b])]
\end{spec}
regroups the neighbouring terms preceded by the same operator
\begin{verbatim}
[(=!=, [b, c]), (===, [d, e]), (=!=, f)]
\end{verbatim}
and everything is combined using a fold.
\subsubsection{Right associative operators}
\begin{code}
data InfixR = Apply | Map | MapAll | Power | Rule | RuleDelayed
  | AddTo | SubtractFrom | TimesBy | DivideBy | UpSet | Set
  | SetDelayed | UpSetDelayed | DoubleSlash
  deriving Eq
\end{code}
Dealing with right associativity is easier than left associativity,
as a simple recursive algorithm is enough to make \inline{InfixR}
an instance of \inline{InfixOp}.
\begin{code}
instance InfixOp InfixR where
  makeExpression x [] = x
  makeExpression x ((DoubleSlash,x'):xs) =
    makeExpression (Cmp x' [x]) xs
  makeExpression x ((op,x'):xs) =
    Cmp (Builtin $ toSymbol op) [x, makeExpression x' xs]
    where
    toSymbol Apply = B.Apply
    toSymbol Map = B.Map
    toSymbol MapAll = B.MapAll
    toSymbol Power = B.Power
    toSymbol Rule = B.Rule
    toSymbol RuleDelayed = B.RuleDelayed
    toSymbol AddTo = B.AddTo
    toSymbol SubtractFrom = B.SubtractFrom
    toSymbol TimesBy = B.TimesBy
    toSymbol DivideBy = B.DivideBy
    toSymbol UpSet = B.UpSet
    toSymbol Set = B.Set
    toSymbol SetDelayed = B.SetDelayed
    toSymbol UpSetDelayed = B.UpSetDelayed
    toSymbol DoubleSlash = error "No builtin symbol associated to //"
\end{code}
\subsubsection{Left associative operators}
\begin{code}
data InfixL = Arobas | Condition | ReplaceAll | ReplaceRepeated
            | PutAppend | Put
              deriving Eq

instance InfixOp InfixL where
  makeExpression x xs = foldl helper x xs
    where
    helper e (Arobas, e') = Cmp e [e']
    helper e (op, e') = Cmp (Builtin $ toSymbol op) [e, e']
    toSymbol Condition = B.Condition
    toSymbol ReplaceAll = B.ReplaceAll
    toSymbol ReplaceRepeated = B.ReplaceRepeated
    toSymbol PutAppend = B.PutAppend
    toSymbol Put = B.Put
    toSymbol Arobas = error "No builtin symbol associated to @"
\end{code}
\subsubsection{Addition}
The operator \verb?'+'? on its own is flat, yet it deserves a
special treatment because of its interaction with \verb?'-'?, see
Table~\ref{parser:tab:addition} for some examples.
\begin{table}[!h]
  \centering
  \begin{tabular}{c|c}
    Input & Parsed expression \\ \hline
    \verb?a + b + c? & \verb?Plus[a, b, c]? \\
    \verb?a - b + c? & \verb?Plus[a, Times[-1, b], c]? \\
    \verb?a - 2 + c? & \verb?Plus[a, -2, c]?
  \end{tabular}
  \caption{parsing additions}
  \label{parser:tab:addition}
\end{table}
\begin{code}
data Addition = Plus | Minus deriving Eq

instance InfixOp Addition where
  makeExpression x [] = x
  makeExpression x xs = Cmp (Builtin B.Plus) (x:(map helper xs))
    where helper (Plus, y) = y
          helper (Minus, Number y) = Number (-y)
          helper (Minus, y) = Cmp (Builtin B.Times) [Number (-1), y]
\end{code}

\subsubsection{Inequalities}
In most languages (for example in C), a succession of inequalities
(\verb?a < b < c?) does not have an obvious meaning.  Fortunately,
this nightmare does not take place with \emph{Kalkulu}. We can even
mix different comparison operators (\verb?a < b <= c?). Each
comparison operator is flat on its own, yet we do not categorize them
in the class of flat operators, because of the possibility to combine
them, see~\ref{parser:tab:inequalities}.
\begin{table}[!h]
  \centering
  \begin{tabular}{c|c}
    Input & Parsed expression \\ \hline
    \verb?a < b < c? & \verb?Less[a, b, c]? \\
    \verb?a < b <= c? & \verb?Inequality[a, Less, b, LessEqual, c]? \\
  \end{tabular}
  \caption{parsing inequalities}
  \label{parser:tab:inequalities}
\end{table}
\begin{code}
data Inequality = Equal | Unequal | Greater | Less | GreaterEqual | LessEqual
                  deriving Eq

instance InfixOp Inequality where
  makeExpression x [] = x
  makeExpression x xs@((op,_):_) = let (ops, es) = unzip xs in
    if all (== op) ops
       then Cmp (toExpr op) (x:es)
       else Cmp (Builtin B.Inequality) args
    where args = x:(concat $ fmap (\(op',e) -> [toExpr op', e]) xs)
          toExpr Equal        = Builtin B.Equal
          toExpr Unequal      = Builtin B.Unequal
          toExpr Greater      = Builtin B.Greater
          toExpr Less         = Builtin B.Less
          toExpr GreaterEqual = Builtin B.GreaterEqual
          toExpr LessEqual    = Builtin B.LessEqual
\end{code}
\section{Operators}
Most unary operators are related to a builtin symbol. We need a function
\inline{toFunc} which realizes the conversion from a symbol to a function.
\begin{code}
toFunc :: B.BuiltinSymbol -> Expr -> Expr
toFunc b e = Cmp (Builtin b) [e]
\end{code}
\subsubsection{List of operators}
\label{parser:subsec:listops}
Finally, we give the operator table. Some operators are explained
below.
\begin{code}
opTable :: [PrecedenceLevel]
opTable = [
  part,
  composite,
  Prefix  (lexeme $ try $ string "<<"  >> return (toFunc B.Get)),
  PatternTest,
  derivative,
  Postfix (lexeme $ try $ (lexeme (char '=') True) >> char '.'
                                       >> return (toFunc B.Unset)),
  Postfix (lexeme $ try $ string "++"  >> return (toFunc B.Increment)),
  Postfix (lexeme $ try $ string "--"  >> return (toFunc B.Decrement)),
  Prefix  (lexeme $ try $ string "++"  >> return (toFunc B.PreIncrement)),
  Prefix  (lexeme $ try $ string "--"  >> return (toFunc B.PreDecrement)),
  Infix   [lexeme $ try $ string "@*"  >> return Composition],
  Infix   [lexeme $ try $ string "@@"  >> return Apply,
           lexeme $ try $ string "/@"  >> return Map,
           lexeme $ try $ string "//@" >> return MapAll],
  Postfix (lexeme $ try $ string "!!"  >> return (toFunc B.Factorial2)),
  Postfix (lexeme $       char   '!'   >> return (toFunc B.Factorial)),
  Infix   [lexeme $ try $ string "<>"  >> return StringJoin],
  Infix   [lexeme $ try $ char   '^'   >> notFollowedBy (oneOf ":=")
                                       >> return Power],
  Infix   [lexeme $ try $ string "**"  >> return NonCommutativeMultiply],
  Infix   [lexeme $ try $ char   '.'   >> notFollowedBy (digit <|> char '.')
                                       >> return Dot],
  Multiplication,
  Infix   [lexeme $ try $ char   '+'   >> notFollowedBy (char '=')
                                       >> return Plus,
           lexeme $ try $ char   '-'   >> notFollowedBy (oneOf ">=")
                                       >> return Minus],
  -- Span,
  Infix   [lexeme $ try $ string "===" >> return SameQ,
           lexeme $ try $ string "=!=" >> return UnsameQ],
  Infix   [lexeme $ try $ string "=="  >> return Equal,
           lexeme $ try $ string "=!"  >> return Unequal,
           lexeme $ try $ string ">="  >> return GreaterEqual,
           lexeme $ try $ string "<="  >> return LessEqual,
           lexeme $ try $ string ">"   >> notFollowedBy (char '>')
                                       >> return Greater,
           lexeme $       string "<"   >> return Less],
  Prefix  (lexeme $       char   '!'   >> return (toFunc B.Not)),
  Infix   [lexeme $ try $ string "&&"  >> return And],
  Infix   [lexeme $ try $ string "||"  >> return Or],
  Postfix (lexeme $ try $ string "..." >> return (toFunc B.RepeatedNull)),
  Postfix (lexeme $ try $ string ".."  >> return (toFunc B.Repeated)),
  Infix   [lexeme $       char   '|'   >> return Alternative],
  Infix   [lexeme $ try $ string "~~"  >> return StringExpression],
  Infix   [lexeme $ try $ string "/;"  >> return Condition],
  Infix   [lexeme $ try $ string "->"  >> return Rule,
           lexeme $ try $ string ":>"  >> return RuleDelayed],
  Infix   [lexeme $ try $ string "/."  >> return ReplaceAll,
           lexeme $ try $ string "//." >> return ReplaceRepeated],
  Infix   [lexeme $       string "+="  >> return AddTo,
           lexeme $       string "-="  >> return SubtractFrom,
           lexeme $       string "*="  >> return TimesBy,
           lexeme $ try $ string "/="  >> return DivideBy],
  Postfix (lexeme $       char   '&'   >> return (toFunc B.Function)),
  Infix   [lexeme $ try $ string "^="  >> return UpSet,
           lexeme $       char   '='   >> return Set,
           lexeme $       string ":= " >> return SetDelayed,
           lexeme $       string "^:=" >> return UpSetDelayed],
  Infix   [lexeme $ try $ string ">>>" >> return PutAppend,
           lexeme $       string ">>"  >> return Put],
  tilde, -- check precedence of operators below
  Infix   [lexeme $       string "@"   >> return Arobas],
  Infix   [lexeme $       string "//"  >> return DoubleSlash],
  CompoundExpression]
\end{code}
\subsubsection{Composite and Part}
In expressions like \verb?a[1, 2]? or \verb?a[[1, 2]]?, the bracketed
parts are treated as postfix operators. This shows that a postfix
operator can be arbitrarily long, so one has to be careful not to
employ the \inline{try} combinator in front of a general postfix. The
function \inline{bracketed} of
subsection~\ref{parser:subsec:bracketed_expressions} will once again
prove useful.
\begin{code}
part :: PrecedenceLevel
part = Postfix $ \ignoreEOL -> do
  args <- bracketed (try $ void $ string "[[")
                    (lexeme (void $ string "]]") ignoreEOL)
  return $ \h -> Cmp (Builtin B.Part) (h:args)

composite :: PrecedenceLevel
composite = Postfix $ \ignoreEOL -> do
  args <- bracketed (void $ char '[')
                    (lexeme (void $ char ']') ignoreEOL)
  return (\h -> Cmp h args)
\end{code}
\subsubsection{Derivative}
As in mathematics, the derivate of a function \verb?f? is \verb?f'?.
It is possible to derivate as many times as wanted (\verb?f'''?,
parsed as \verb?Derivate[3][f]?), giving birth to an infinite family
of postfix operators.
\begin{code}
derivative :: PrecedenceLevel
derivative = Postfix $ \ignoreEOL -> do
  n <- toInteger . length <$> (many1 $ (lexeme (char '\'') ignoreEOL))
  return $  \x -> Cmp (Cmp (Builtin B.Derivative) [Number n]) [x]
\end{code}
\subsubsection{Tilde}
Any symbol \verb?f? can be turned as a flat infix operator by
surrounding it with \verb?'~'?. In this case, \verb?~f~? as a whole
is considered as an operator.
\begin{code}
tilde :: PrecedenceLevel
tilde = Infix [\ignoreEOL -> do void $ lexeme (char '~') True
                                s <- lexeme identifier True
                                void $ lexeme (char '~') ignoreEOL
                                return $ Tilde s]
\end{code}
\end{document}
