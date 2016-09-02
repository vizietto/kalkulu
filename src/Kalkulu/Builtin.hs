module Kalkulu.Builtin where

import Data.Array (Ix)

data BuiltinSymbol =
  AddTo
  | All
  | Alternative
  | And
  | Apply
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
  | Factorial
  | Factorial2
  | Function
  | Get
  | Greater
  | GreaterEqual
  | Increment
  | Inequality
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
  | StringExpression
  | StringJoin
  | SubtractFrom
  | Times
  | TimesBy
  | Unequal
  | UnsameQ
  | Unset
  | UpSet
  | UpSetDelayed
  deriving (Bounded, Enum, Eq, Ix, Ord, Show)
