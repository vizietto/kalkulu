module Kalkulu.BuiltinSymbol where

import Data.Array (Ix)

data BuiltinSymbol =
    AddTo
  | All
  | Alternative
  | And
  | Apply
  | AtomQ
  | Blank
  | BlankNullSequence
  | BlankSequence
  | Condition
  | Composition
  | CompoundExpression
  | Decrement
  | Derivative
  | DivideBy
  | Dot
  | Equal
  | Evaluate
  | Factorial
  | Factorial2
  | False
  | Function
  | Get
  | Greater
  | GreaterEqual
  | Head
  | Hold
  | If
  | Increment
  | Indeterminate
  | Inequality
  | Integer
  | Length
  | Less
  | LessEqual
  | List
  | Map
  | MapAll
  | MessageName
  | NonCommutativeMultiply
  | Not
  | Null
  | Optional
  | Or
  | Out
  | Part
  | Pattern
  | PatternTest
  | Plus
  | PreDecrement
  | PreIncrement
  | Power
  | Put
  | PutAppend
  | Repeated
  | ReplaceRepeated
  | RepeatedNull
  | ReplaceAll
  | Rule
  | RuleDelayed
  | RuleRepeated
  | SameQ
  | Sequence
  | Set
  | SetDelayed
  | Slot
  | SlotSequence
  | Span
  | String
  | StringExpression
  | StringJoin
  | SubtractFrom
  | Symbol
  | Times
  | TimesBy
  | True
  | Unequal
  | UnsameQ
  | Unset
  | UpSet
  | UpSetDelayed
  deriving (Bounded, Enum, Eq, Ix, Ord, Show)
