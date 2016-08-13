module Kalkulu.Builtin where

data BuiltinSymbol =
    Blank
  | BlankNullSequence
  | BlankSequence
  | List
  | MessageName
  | Null
  | Optional
  | Out
  | Part
  | Pattern
  | Slot
  | SlotSequence  
  deriving (Eq, Show)