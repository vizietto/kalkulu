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
decomposition presented in Equation~\eqref{parser:eq:SE}.  From now
on, we will work only with lexemized parsers, \emph{i.e} parsers
which consume trailing space. Remember that an end of line character
can be considered as white space, depending on the context.  All
lexemized parsers have consequently one boolean argument indicating
whether or not to ignore end of lines.
\begin{code}
simpleExpr :: Bool -> Parser Expr
simpleExpr ignoreEOL = do
  h <- lexeme ignoreEOL $ vse
  t <- many $ lexeme ignoreEOL $ (part <|> cmp)
  return $ foldl (flip ($)) h t 
\end{code}

\section{Operator precedence parser}
\subsection{Algorithm}
\label{parser:sub:algorithm}
It is now possible to parse simple expressions. The strategy for
parsing general expressions is to design a ``higher-order''
combinator
\begin{spec}
makeParser :: (Bool -> Parser Expr) -> OperatorList -> (Bool -> Expr)
\end{spec}
which does the following:
\begin{itemize}
\item it takes a lexemized parser \inline{Bool -> Parser Expr} as an
  argument (the argument of type \inline{Bool} inside the parser
  indicates whether or not to ignore end of lines inside and after an
  expression),
\item a list of operators \verb?listop? of the same precedence,
\item it finally returns an improved parser, now able to parse
  operators from the list \verb?listop?.
\end{itemize}
The final parser is built by progressively enriching the simple
expression parser with operators of decreasing precedence
\begin{code}
expr :: Bool -> Parser Expr
expr = foldl makeParser simpleExpr opTable
\end{code}
where \inline{opTable :: [OperatorList]} is table of all operators,
documented in section~\ref{parser:sec:listops}.

The following representation of operators with same precedence
forbids us to mix different types of associativity.
\begin{code}
data OperatorList =
    InfixL    [Parser B.BuiltinSymbol]
  | InfixR    [Parser B.BuiltinSymbol]
  | InfixF    [Parser B.BuiltinSymbol]
  | InfixN    (Parser B.BuiltinSymbol)
  | Prefix    [Parser B.BuiltinSymbol]
  | Postfix   [Parser B.BuiltinSymbol]
  | forall a. SpecialOp a => SpecialInfix [Parser a]
  | CompoundExpression
  | Derivative
  | Multiplication
  | Span

class (Eq a) => SpecialOp a where
  makeExpression :: Expr -> [(a, Expr)] -> Expr
\end{code}
To any well-behaved operator is associated a parser of type
\inline{Parser B.BuiltinSymbol} (parsing the operator and returning
it as a symbol). For example, the parser for \verb?'!'? is
\begin{spec}
  char '!' >> return B.Factorial
\end{spec}
The constructors \inline{InfixL}, \inline{InfixR} and \inline{InfixF}
respectively accept lists of left, right and flat associative infix
operators.  The constructor \inline{InfixN} takes a single non
associative operator. All constructors from \inline{SpecialInfix} are
there to deal with exceptions and will be explained in
subsection~\ref{parser:subsec:special_cases}.

For the following sections, we need a helper function
\inline{processOps}, which transforms a list of operator parsers into
a single parser.
\begin{code}
processOps :: [Parser a] -> Parser a
processOps = choice . liftM try
\end{code}

\subsection{Infix operators}
Suppose we have a parser \verb?parser? able to parse expressions
involving operators of precedence \emph{stricly} higher than some
\inline{OperatorList}, let us take for example the operator list
consisting of \verb?===? and \verb?=!=?  (\verb?SameQ? and
\verb?UnsameQ?, see section~\ref{parser:sec:listops}).

Now, we would like to parse expressions like
\begin{equation}
  \label{parser:eq:infix_ops}
\verb?expr?_0 \,\, \verb?op?_1 \,\, \verb?expr?_1 \,\, \verb?op?_2
\,\, \cdots \,\, \verb?op?_n \,\, \verb?expr?_n,
\end{equation}
where $\verb?op?_i \in \{\verb?===?, \verb?=!=?\}$ and the
expressions inbetween $\verb?expr?_i$ (for $0 \leq i \leq n-1$)
are parseable by \verb?parser?. A moment of reflexion convinces us
that the last expression $\verb?expr?_n$ may begin with a prefix
operator of lower precedence than $\verb?op?_n$. In this case, this
expression needs to be parsed with a special parser.

For example, in the expression \verb?a === !b + c? (parsed as
\verb?SameQ[a, Not[Plus[b, c]]]?),
\begin{itemize}
\item the left hand side \verb?a? is parsed by \verb?parser?
  (who knows all operators of precedence strictly greater than
  \verb?===? and \verb?=!=?).
\item the right hand side is parsed by the parser knowning all
  operators of precedence greater or equal than \verb?'!'?.
\end{itemize}
A function collecting all operators in~\eqref{parser:eq:infix_ops}
is strongly needed. This function would
decompose~\eqref{parser:eq:infix_ops} into
\[
  (\verb?expr?_0, [(\verb?op?_1, \verb?expr?_1), \dots,
  (\verb?op?_n, \verb?expr?_n)]).
\]
Thus we introduce
\begin{code}
collect :: (Bool -> Parser Expr) -> Parser a -> Bool -> Parser (Expr, [(a, Expr)])
collect parser op ignoreEOL =
  (,) <$> parser ignoreEOL <*> many ((,) <$> op' <*> parser')
  where parser' = parser ignoreEOL <|> prefixParser ignoreEOL
        op' = lexeme True op
\end{code}
The parser \inline{prefixParser :: Parser Expr} does the job for
the last expression. It tries to read a prefix operator (if it
fails here, it does not consume anything), then reads what follows
according to this prefix.
\begin{code}
prefixParser :: Bool -> Parser Expr
prefixParser ignoreEOL = choice $ map f listPrefixParsers
  where
  listParsers = tail $ scanl makeParser simpleExpr opTable
  listPrefixParsers = filter (isPrefix . fst) (zip opTable listParsers)
  f (op, parser) = (opToParser op) >> parser ignoreEOL
  isPrefix (Prefix _) = True
  isPrefix Span       = True
  isPrefix _          = False
  opToParser (Prefix ops) = void $ choice $ liftM lookAhead $ ops
  opToParser Span = void $ lookAhead $ string $ ";;"
\end{code}
Some infix operators do not belong to a well-defined associativity
class, yet are regular enough to considered as infix operators.  The
data constructor \inline{SpecialInfix} is dedicated to them.  The
member function \inline{makeExpression} of the class
\inline{SpecialOp} specifies how to combine operators and expressions
collected by \inline{collect}.
\begin{code}
makeParser :: (Bool -> Parser Expr) -> OperatorList -> Bool -> Parser Expr
makeParser parser (SpecialInfix ops) ignoreEOL =
  (uncurry makeExpression) <$> collect parser (processOps ops) ignoreEOL
\end{code}

\begin{code}
makeParser parser (InfixL ops) ignoreEOL = do
  (x, rest) <- collect parser (processOps ops) ignoreEOL
  return $ foldl (flip ($)) x (map f rest)
  where f (op, y) = \e -> Cmp (Builtin op) [e, y]
\end{code}
%%%%%%%%%%%%%%%%%%%%%%
Several flat associative operators can share the same precedence
(the most important case being \verb?===? and \verb?=!=?, operators
associated to the symbols \verb?SameQ? and \verb?UnsameQ?), see
Table~\ref{parser:tab:flat_operators} to discover what happens in
this case.
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
makeParser parser (InfixF ops) ignoreEOL = do
  (x, rest) <- collect parser (processOps ops) ignoreEOL
  return $ foldl (\e (op, es) -> Cmp (Builtin op) (e:es)) x (helper rest)
  where helper []     = []
        helper ((op, e):xs) = case (helper xs) of
          []              -> [(op, [e])]
          l@((op', es):t) -> if op == op' then (op, e:es):t else (op, [e]):l
\end{code}
Let us see what happens in the second example of
Table~\ref{parser:tab:flat_operators}. The variable \verb?x? is
bound to \verb?a?, whereas \verb?rest? is bound to
\verb?[(=!=, b), (=!=, c), (===, d), (===, e), (=!=, [f])]?.
Then, applying
\verb?rest? to the helper function
\begin{spec}
helper :: Eq a => [(a, b)] -> [(a, [b])]
\end{spec}
regroups the neighbouring terms preceded by the same operator
\begin{verbatim}
[(=!=, [b, c]), (===, [d, e]), (=!=, f)]
\end{verbatim}
and everything is combined using a fold.

It is also possible to parse non associative operators, using
\inline{InfixN}. Actually, the only non associative operator in
\emph{Kalkulu} is \verb!'?'! (\verb?PatternTest?). Nevertheless, we
provide a general way to parse such operators in case the grammar
evolves.
\begin{code}
makeParser parser (InfixN op) ignoreEOL = do
  x <- parser ignoreEOL
  (do symbol <- lexeme True $ try $ op
      y <- parser ignoreEOL
      notFollowedBy op
      return $ Cmp (Builtin symbol) [x, y])
    <|> return x
\end{code}
We end this subsection with right associative infix operators.
We will not use the decomposition~\eqref{parser:eq:infix_ops},
instead we can write a recursive parser using
\begin{center}
\texttt{<seqOfTerms> ::= <term> | <term> <op> <seqOfTerm>}
\end{center}
\begin{code}
makeParser parser oplist@(InfixR ops) ignoreEOL = do
  x <- parser ignoreEOL
  (do infixOp <- lexeme True $ processOps ops
      y <- makeParser parser oplist ignoreEOL
      return $ Cmp (Builtin infixOp) [x, y])
    <|> return x
\end{code}
\subsection{Unary operators}

\subsection{Special cases}
\label{parser:subsec:special_cases}

\subsubsection{CompoundExpression}
The infix operator \verb?;? has a peculiarity: its arguments may be
implicit \verb?Null?s, except for the first one: \verb?"a;"? is
parsed \verb?CompoundExpression[a, Null]}?. Beware that \verb?";;"?
(the infix notation of \verb?Span?) is not the same as \verb?"; ;"?.
However, we need not check the character after \verb?;? because
\verb?Span? has a higher precedence that \verb?CompoundExpression?.
\begin{code}
makeParser parser CompoundExpression ignoreEOL = do
  x     <- parser ignoreEOL
  rest  <- many $ (op >> expr)
  return $ if null rest
     then x
    else Cmp (Builtin B.CompoundExpression) (x:rest)
  where expr = parser ignoreEOL <|> return (Builtin B.Null)
        op   = lexeme ignoreEOL $ (char ';')
\end{code}

\subsubsection{Derivative}
Derivation, as in mathematics, is denoted by a single quote \verb?'?.
It is possible to derivate as many times as desired (\verb?f''''?),
giving birth to an infinite family of postfix operators.
\begin{code}
makeParser parser Derivative ignoreEOL = do
  x <- parser ignoreEOL
  n <- toInteger . length <$> (many $ (lexeme ignoreEOL $ char '\''))
  return $ if n == 0 then x
             else Cmp (Cmp (Builtin B.Derivative) [Number n]) [x]
\end{code}

\subsubsection{Multiplication}

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
makeParser parser Span ignoreEOL = times <$> extract <$> (many1 opOrExpr)
  where
  op = try $ lexeme ignoreEOL $ string ";;"
  opOrExpr :: Parser (Either () Expr)
  opOrExpr = (op >> (return $ Left ())) <|> (Right <$> parser ignoreEOL)
  extract (Right a : Left _ : Right b : Left _ : Right step : xs) =
    (span' [a, b, step]):(extract xs)                                -- Case 5
  extract (Right a : Left _ : Right b:xs) =
    (span' [a, b]):(extract xs)                                      -- Case 1
  extract (Right a : Left _ : Left _ : Right step : xs) =
    (span' [a, all', step]):(extract xs)                             -- Case 6
  extract (Right a : Left _ : xs) = (span' [a, all']):(extract xs)   -- Case 2
  extract (Right _ : Right _ : _) =
    error "unreachable: implicit Times already taken care of"
  extract (Right a:xs) = a:(extract xs)                              -- No ;;
  extract (Left _ : Right b : Left _ : Right step : xs) =
    (span' [one, b, step]):(extract xs)                              -- Case 7
  extract (Left _ : Right b : xs) = (span' [one, b]):(extract xs)    -- Case 3
  extract (Left _ : Left _ : Right step : xs) =
    (span' [one, all', step]):(extract xs)                           -- Case 8
  extract (Left _ : xs) = (span' [one, all']):(extract xs)           -- Case 4
  extract [] = []
  times []  = error "unreachable: parser Span parses something or fails"
  times [x] = x
  times xs  = Cmp (Builtin B.Times) xs
  span' = Cmp (Builtin B.Span)
  one   = Number 1
  all'  = Builtin B.All
\end{code}

\subsubsection{Addition}
The operator \verb?'+'? on its own is flat, yet we categorize
it as a \inline{SpecialInfix} because of its interaction with
\verb?'-'?, see Table~\ref{parser:tab:addition} for some examples.
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

instance SpecialOp Addition where
  makeExpression x [] = x
  makeExpression x xs = Cmp (Builtin B.Plus) (x:(fmap doMinus xs))
    where doMinus (Plus, y) = y
          doMinus (Minus, Number i) = Number (-i)
          doMinus (Minus, y) = Cmp (Builtin B.Times) [Number (-1), y]
\end{code}

\subsubsection{Inequalities}
Each comparison operator is flat on its own, but something strange
happens when we mix them together, see~\ref{parser:tab:inequalities}.
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

instance SpecialOp Inequality where
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

\section{List of operators}
\label{parser:sec:listops}
\begin{code}
opTable :: [OperatorList]
opTable = [
    Prefix       [string "<<"  >> return B.Get],
    InfixN       (char   '?'   >> return B.PatternTest),
    Derivative,
    Postfix      [char   '='   >> (lexeme True $ char '.')
                               >> return B.Unset],
    Postfix      [string "++"  >> return B.Increment,
                  string "--"  >> return B.Decrement],
    Prefix       [string "++"  >> return B.PreIncrement,
                  string "--"  >> return B.PreDecrement],
    InfixF       [string "@*"  >> return B.Composition],
    InfixR       [string "@@"  >> return B.Apply,
                  string "/@"  >> return B.Map,
                  string "//@" >> return B.MapAll],
    Postfix      [string "!!"  >> return B.Factorial2,
                  char   '!'   >> return B.Factorial],
    InfixF       [string "<>"  >> return B.StringJoin],
    InfixR       [char   '^'   >> return B.Power],
    InfixF       [string "**"  >> return B.NonCommutativeMultiply],
    InfixF       [char   '.'   >> notFollowedBy digit
                               >> return B.Dot],
--    Multiplication,
    SpecialInfix [char   '+'   >> return Plus,
                  char   '-'   >> return Minus],
    Span,
    InfixF       [string "===" >> return B.SameQ,
                  string "=!=" >> return B.UnsameQ],
    SpecialInfix [string "=="  >> return Equal,
                  string "=!"  >> return Unequal,
                  string ">="  >> return GreaterEqual,
                  string "<="  >> return LessEqual,
                  char   '>'   >> return Greater,
                  char   '<'   >> return Less],
    Prefix       [char   '!'   >> return B.Not],
    InfixF       [string "&&"  >> return B.And],
    InfixF       [string "||"  >> return B.Or],
    Postfix      [string "..." >> return B.RepeatedNull,
                  string ".."  >> return B.Repeated],
    InfixF       [char '|'     >> return B.Alternative],
    InfixF       [string "~~"  >> return B.StringExpression],
    InfixL       [string "/;"  >> return B.Condition],
    InfixR       [string "->"  >> return B.Rule,
                  string ":>"  >> return B.RuleDelayed],
    InfixL       [string "/."  >> return B.ReplaceAll,
                  string "//." >> return B.ReplaceRepeated],
    InfixR       [string "+="  >> return B.AddTo,
                  string "-="  >> return B.SubtractFrom,
                  string "*="  >> return B.TimesBy,
                  string "/="  >> return B.DivideBy],
    Postfix      [char   '&'   >> return B.Function],
    InfixR       [string "^="  >> return B.UpSet,
                  char   '='   >> return B.Set,
                  string ":="  >> return B.SetDelayed,
                  string "^:=" >> return B.UpSetDelayed],
    InfixL       [string ">>>" >> return B.PutAppend,
                  string ">>"  >> return B.Put],
    CompoundExpression]    
\end{code}
\end{document}
