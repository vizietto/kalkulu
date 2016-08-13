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

import Kalkulu.Builtin as B
\end{code}

\section{Overview of the grammar}
\label{parser:sec:overview}

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
  \emph{flat} associativity. The
  table~\ref{parser:tab:associativity_types} shows how a string
  \verb?"a op b op c"? is parsed, for an operator \verb?op?.
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
    (VSE1)  & Number   & \verb?123? & \verb?123? \\ \hline
    (VSE2)  & String   & \verb?"abc"? & \verb?"abc"? \\ \hline
    (VSE3a) &          & \verb?_?     & \verb?Blank[]? \\
    (VSE3b) &          & \verb?_s? & \verb?Blank[s]? \\
    (VSE3c) &          & \verb?_.?    & \verb?Optional[Blank[]]? \\
    (VSE3d) & Blanks   & \verb?__?    & \verb?BlankSequence[]? \\
    (VSE3e) &          & \verb?__s? & \verb?BlankSequence[s]? \\
    (VSE3f) &          & \verb?___? & \verb?BlankNullSequence[]? \\
    (VSE3g) &          & \verb?___s? & \verb?BlankNullSequence[s]? \\ \hline
    (VSE4a) & Symbol   & \verb?s? & \verb?s? \\ \hline
    (VSE4b) &          & \verb?s_? & \verb?Pattern[s, Blank[]]? \\
    (VSE4c) &          & \verb?s1_s2? & \verb?Pattern[s1, Blank[s2]]? \\
    (VSE4d) &          & \verb?s_.? & \verb?Optional[Pattern[s, Blank[]]]? \\
    (VSE4e) & Patterns &  \verb?s__? & \verb?Pattern[s, BlankSequence[]]? \\
    (VSE4f) & &\verb?s1__s2? & \verb?Pattern[s1, BlankSequence[s2]]? \\
    (VSE4g) & &\verb?s___? & \verb?Pattern[s, BlankNullSequence[]]? \\
    (VSE4h) & & \verb?s1___s2? & \verb?Pattern[s1, BlankNullSequence[s2]]? \\
    \hline
    (VSE4i) & \multirow{2}{*}{Message}
                      & \verb?s1::s2? & \verb?MessageName[s1, "s2"]? \\
    (VSE4j) &         & \verb?s1::s2::s3?
                      & \verb?MessageName[s1, "s2", "s3"]? \\ \hline
    (VSE5a) &         & \verb?#? & \verb?Slot[1]? \\
    (VSE5b) &         & \verb?#3? & \verb?Slot[3]? \\
    (VSE5c) & Slots   & \verb?#abc? & \verb?Slot["abc"]? \\
    (VSE5d) &         & \verb?##? & \verb?SlotSequence[1]? \\
    (VSE5e) &         & \verb?##3? & \verb?SlotSequence[3]? \\ \hline
    (VSE6a) &         & \verb?%? & \verb?Out[]? \\
    (VSE6b) & Out     & \verb?%%%%? & \verb?Out[-4]? \\
    (VSE6c) &         & \verb?%5? & \verb?Out[5]? \\ \hline
    (VSE7) & Paren.   & \verb?(expr)? & \verb?expr? \\ \hline
    (VSE8) & List     & \verb?{expr, ...}? & \verb?List[expr, ..]? \\ \hline
    (SE1)  & Cmp      & \verb?simpleExpr[expr, ...]?
                               & \verb?simpleExpr[expr, ...]? \\ \hline
    (SE2) & Part      & \verb?simpleExpr[[expr, ...]]?
                               & \verb?Part[simpleExpr, expr, ...]? \\ \hline
  \end{tabular}
  \caption{Simple expressions}
  \label{parser:tab:simple_expressions}
\end{table}
Unfortunately, the definition of a simple expression is
left-recursive because of (SE1) and (SE2). We can circumvent the
issue easily. Let us call a \emph{very simple expression} a simple
expression of type (VSE1) to (VSE8). Then, any simple expression has
the form
\begin{equation}
  \label{parser:eq:SE}
  \verb?verySimpleExpr?\left(\left[\left(\verb?expr?(\verb?, expr?)^*
  \right)^? \right] \mid \left[\left[\left(\verb?expr?(\verb?, expr?)^*
  \right)^? \right]\right]  \right)^*
\end{equation}
Displaying (the inverse problem of parsing) requires also to know the
precedences of operators. \emph{Mathematica} provides a function
\verb?Precedence? for that. The boolean operators have the same
precedence (equal to \verb?215.?), but this concerns only displaying.
When we read \verb?a || b && c?, we perfectly know that \verb?And?
  is computed before \verb?||?, because we are used to. Putting extra
  parentheses around \verb?b && c? would be bothersome. The moral is
  that we should not rely too much on \verb?Precedence? in this
  chapter.

Here are some motivations on how to discriminate between operators,
simple expressions, etc. First, let us call \emph{lexeme} an
expression possibly followed by whitespace.
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
  compound expressions of type (VSE7), (VSE8), (SE1) and (SE2).
\end{itemize}

\section{Simple expression parser}
Parsing in \emph{Kalkulu} is an impure process. For example, to
parse a symbol \verb?symb?, one needs to know the current context,
and this information is only available at run time. Even if knew it,
it is also necessary to put new symbols in the symbol table.

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
the context. The argument of the following never failing parser
indicates whether or not to ignore end of lines.
\begin{code}
whitespace :: Bool -> Parser ()
whitespace ignoreEOL = skipMany $ (space <|> comment)
  where space = void $ satisfy isSpace'
        isSpace' c = isSpace c && (ignoreEOL || c /= '\n')
\end{code}
Comments are delimited by \verb?(*? and \verb?*)? and can be nested.
\begin{code}
comment :: Parser ()
comment = do void $ try $ string "(*"
             skipMany $ (void (noneOf "(*") <|> star <|> par <|> comment)
             void $ string "*)"
             where star = try $ void $ char '*' >> notFollowedBy (char ')')
                   par  = try $ void $ char '(' >> notFollowedBy (char '*')
\end{code}
One should not use the \inline{whitespace} very often (mainly at the
beginning of the main parser to skip any leading white space).
Instead, the following function proves to be useful.
\begin{code}
lexeme :: Bool -> Parser a -> Parser a
lexeme ignoreEOL p = do x <- p
                        whitespace ignoreEOL
                        return x
\end{code}

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
  to be followed by a digit, see section~\textbf{TODO}.}. This
property is important for fast parsing, because a simple expression
can be followed by either a simple expression or an operator (an
outcome of implicit multiplication). Also, unary operator \verb?'-'?
  is smart enough to parse \verb?-2? as an atom rather than
  \verb?Times[-1, 2]?.

\subsection{Strings}
A \verb?String? is enclosed by quotes \verb?'\"'?. The escape
characters \verb?'\n'?, \verb?'\t'?, \verb?'\\'? and \verb?'\"'? are
allowed. As the combinator \inline{string} already exists in
\verb?Parsec?, we call our own combinator \inline{string'}. It
parses very simple expressions of type (VSE2).
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
mays contain any alphanumeric character, or \verb?'$'?.
Additionally, a symbol may contain the character \verb?'`'?
  (backquote) to separate its proper name from its context
name(s). Contexts can be nested in one another. The full name of a
symbol involves a sequence of context names
\verb?context1`context2`...`name?. A symbol identifier can also begin
with \verb?'`'?. In this case, it means that the symbol shoud be
searched within the current context.

In any case, an identifier cannot end with \verb?'`'?, and it cannot
contain two consecutive backquotes. A backquote is necessarily
followed by another name.
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
Finally, we can parse very simple expressions of type (VSE4a).
\begin{code}
symbol :: Parser Expr
symbol = Symbol <$> identifier
\end{code}

\subsection{Blanks, Patterns and Messages}
In this subsection, we first parse expressions of type (VSE3).
Because of the similarity with the expressions (VSE4b) -- (VSE4h),
this part will be used to parse patterns as well.  The first symbol
(left of \verb?_? in (VSE4b) -- (VSE4h)) is called the \emph{pattern}
symbol.

We will define a function
\begin{spec}
blank :: Maybe Expr -> Parser Expr
\end{spec}
which takes as an argument a previously parsed pattern symbol, or
nothing in case of an anonymous blank, and parses the whole
expression.

To parse an anonymous blank (VSE3), simply use
\inline{blank Nothing}.
\begin{code}
blank :: Maybe Expr -> Parser Expr
blank p = char '_' >> (
  (try (string "__") >> (vse3g <|> vse3f))
  <|> (char '_' >> (vse3e <|> vse3d))
  <|> try vse3c <|> vse3b <|> vse3a)
  where vse3a = makeBlank B.Blank
        vse3b = makeBlank' B.Blank
        vse3c = do void $ char '.'
                   notFollowedBy (char '.')
                   return $ Cmp (Builtin B.Optional)
                                [makePattern $ Cmp (Builtin B.Blank) []]
        vse3d = makeBlank  B.BlankSequence
        vse3e = makeBlank' B.BlankSequence
        vse3f = makeBlank  B.BlankNullSequence
        vse3g = makeBlank' B.BlankNullSequence
        makePattern arg = case p of
          Nothing -> arg
          Just x  -> Cmp (Builtin B.Pattern) [x, arg]
        makeBlank b = return $ makePattern $ Cmp (Builtin b) []
        makeBlank' b = do
          s <- symbol
          return $ makePattern $ Cmp (Builtin b) [s]
\end{code}
Note that in the case (VSE3c), the character \verb?'.'? cannot be
followed by another dot, because the expression \verb?_..? is parsed
as \verb?Repeated[Blank[]]? (here \verb?..? is as a postfix
operator). In order to correctly parse this expression, we need to
put a \inline{try} in front of this litigious case.

It remains to parse expressions of type (VSE4i). The syntax
\verb?s1::s2? is peculiar: \verb?s2? is parsed as a string, though
it is not delimited by quotes. Moreover, \verb?s2? has the syntax
of a symbol identifier, without backquotes. This holds also for
expressions \verb?#abc? of type (VSE5c) (except that the identifier
\verb?abc? can contain backquotes).

The following function \inline{vse4i} parses from
\verb?::s2(::s3)?, assuming that the symbol \verb?s1? has been
previously parsed and passed as an argument.
\begin{code}
vse4ij :: Expr -> Parser Expr
vse4ij s1 = do
  void $ try $ string "::"
  s2 <- identifier
  (do void $ try $ string "::"                                       -- VSE4j
      s3 <- identWithoutBackquote
      return $ Cmp (Builtin B.MessageName) [s1, String s2, String s3])
   <|> (return $ Cmp (Builtin B.MessageName) [s1, String s2])        -- VSE4i
  where identWithoutBackquote = (:) <$> letter <*> many alphaNum
\end{code}
Note the \inline{try} combinator in front of every occurence of
\verb?string "::"?. This is because \verb?::? should not be confused
with the infix operator \verb?:?(representing \verb?Pattern?).  Now,
we are equipped to parse simple expressions of type (VSE4a) -- (VSE4j).
\begin{code}
vse4 :: Parser Expr
vse4 = symbol >>= \s ->
  (notFollowedBy ((void $ char '_') <|> (void $ string "::")) >> return s)
  <|> (blank $ Just s)
  <|> vse4ij s
\end{code}
In \emph{Mathematica}, the syntax is slightly more permissive,
expressions of type \verb?simpleExpr::s? are allowed.  This makes the
definition of simple expressions left-recursive.  Moreover, this
complication only enables to write only non sensical
expressions. There is no restriction in \emph{Kalkulu} because those
expressions can still be written in \verb?FullForm?.
\subsection{Slots and Outs}
The following parses expressions of type (VSE5) and (VSE6).
\begin{code}
slot :: Parser Expr
slot = char '#' >> (
  (do i <- natural
      return $ Cmp (Builtin B.Slot) [i])                             -- VSE5b
  <|> (do s <- identifier
          return $ Cmp (Builtin B.Slot) [String s])                  -- VSE5c
  <|> (char '#' >> ((do i <- natural                                 -- VSE5e
                        return $ Cmp (Builtin B.SlotSequence) [i])
                    <|> (return $ Cmp (Builtin B.SlotSequence) []))) -- VSE5d
  <|> (return $ Cmp (Builtin B.Slot) []))                            -- VSE5a

out :: Parser Expr
out = char '%' >> (
  (do i <- natural
      return $ Cmp (Builtin B.Out) [i])                              -- VSE6c
  <|> (do i <- toInteger <$> length <$> many (char '%')
          return $ case i of
             0 -> Cmp (Builtin B.Out) []                             -- VSE6a
             _ -> Cmp (Builtin B.Out) [Number (-i-1)]))              -- VSE6b
\end{code}
\subsection{Bracketed expressions}
The following parses parenthesized expressions (type (VSE7)). A
general pattern is that when we deal with delimited expressions, the
opening token (here \verb?'('?)  has to be lexemized. Spaces
(including end of lines) are ignored \emph{before}, \emph{inside} and
\emph{after} the parenthesized expression (\inline{expr True} is
already lexemized, see subsection~\ref{parser:sub:algorithm}, it
would be redundant to replace it with \inline{lexeme True $ expr
  True}).
\begin{code}
parenthesizedExpr :: Parser Expr
parenthesizedExpr = between (lexeme True $ char '(') (char ')') (expr True)
\end{code}
Among delimited expressions, expressions of type (VSE7) are
exceptions because they can contain only one subexpression (\verb?()?
and \verb?(e1, e2)? provoke a failure).

Next, we write a general function to parse a succession of comma
separated expressions.  We take care of implicit \verb?Null? symbols,
which slightly obfuscates the code (we cannot use a simple
\inline{sepBy} because we do not want an empty list \verb?{}? to be
parsed as \verb?List[Null]?).
\begin{code}
bracketed :: Parser () -> Parser () -> Parser [Expr]
bracketed opening closing = do
  lexeme True $ opening
  first <- firstArg
  rest  <- many $ commaArg
  closing
  return $ case (first, rest) of
    (Nothing, []) -> []
    (Nothing, xs) -> (Builtin B.Null) : xs
    (Just x , xs) -> x : xs
  where firstArg = optionMaybe (lexeme True $ expr True)
        arg = lexeme True (expr True <|> implicitNull)
        commaArg = (lexeme True $ char ',') >> arg
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
of very simple expressions.
\begin{code}
vse :: Parser Expr
vse = natural <|> string' <|> blank Nothing
      <|> vse4 <|> slot <|> out
      <|> parenthesizedExpr <|> list
\end{code}
No \inline{try} combinator is needed because each of the eight
categories of very simple expressions has a proper beginning letter:
\begin{itemize}
\item numbers begin with \verb?'.'? or a digit,
\item strings begin with \verb?'"'?,
\item blanks begin with \verb?'_'?,
\item expressions of type (VSE4) begin with \verb?'$'?, \verb?'`'? or
  a letter,
\item slots begin with \verb?'#'?,
\item outs begin with \verb?'%'?,
\item parenthesized expressions begin with \verb?'('?,
\item lists begin with \verb?'{'?.
\end{itemize}
  
The \inline{bracketed} parser will once again proves useful.
To parse a composite expression \verb?h[args, ...]?, we start
to parse at the opening square bracket (assuming the head was parsed
before). We return the function mapping the head \verb?h? to the
well formed composite expression. We use a similar technique for
expressions of type (SE2).
\begin{code}
cmp :: Parser (Expr -> Expr)
cmp = do
  args <- bracketed (void $ char '[') (void $ char ']')
  return (\h -> Cmp h args)

part :: Parser (Expr -> Expr)
part = do
  args <- bracketed (try $ void $ string "[[") (void $ string "]]")
  return $ \h -> Cmp (Builtin B.Part) (h:args)
\end{code}
Finally, the parser for simple expressions is based on the
decomposition presented in Equation~\eqref{parser:eq:SE}.
\begin{code}
simpleExpr :: Parser Expr
simpleExpr = do
  h <- vse
  t <- many (part <|> cmp)
  return $ foldl (flip ($)) h t 
\end{code}

\section{Operator precedence parser}
\subsection{Algorithm}
\label{parser:sub:algorithm}

\begin{code}
-- temporary
expr :: Bool -> Parser Expr
expr = const simpleExpr
\end{code}
\end{document}
