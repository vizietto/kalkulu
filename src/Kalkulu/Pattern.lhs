\documentclass[main.tex]{subfiles}

\begin{code}
module Kalkulu.Pattern where

import Kalkulu.Expression
import Kalkulu.Kernel
import Kalkulu.Symbol
import qualified Kalkulu.BuiltinSymbol as B
import qualified Data.Vector as V

-- Expected behaviour
-- SetAttributes[f, Orderless]
-- MatchQ[f[a, b], HoldPattern[f[b, _]]]      ===> True
-- MatchQ[f[a, b], HoldPattern[f[b, a]]]      ===> False

data Pattern = Blank
             | BlankSequence
             | BlankNullSequence
             | HeadedBlank             Expression
             | HeadedBlankSequence     Expression
             | HeadedBlankNullSequence Expression
             | Pattern                 Symbol Pattern
             | Alternative             [Pattern]
             | Expression              Expression
             | PatternCmp              [Pattern]

instance ToExpression Pattern where
  toExpression Blank                       = CmpB B.Blank []
  toExpression BlankSequence               = CmpB B.BlankSequence []
  toExpression BlankNullSequence           = CmpB B.BlankNullSequence []
  toExpression (HeadedBlank h)             = CmpB B.Blank [h]
  toExpression (HeadedBlankSequence h)     = CmpB B.BlankSequence [h]
  toExpression (HeadedBlankNullSequence h) = CmpB B.BlankNullSequence [h]
  toExpression (Pattern s p)    = CmpB B.Pattern [s, toExpression p]
  toExpression (Alternative es) = CmpB B.Alternative (V.fromList es)
  toExpression e                = e -- TODO: Verbatim is e contains pattern
\end{code}  
The purpose of pattern matching is to find, for example

Expression          Pattern               Bindings
f[a, b]             _[x_, y_]             [[(x, a), (y, a)]]
f[a, b, c]          f[x__, y__]           [[(x, a), (y, Sequence[b, c])],
                                           [(x, Sequence[a, b]), (y, c)]]
f[a, f[a, b, c]]    f[x_, f[x__, y__]]    [[(x, a), (y, Sequence[b, c])]]

Case 2 shows that several solutions can arise
Case 3 shows that 
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
