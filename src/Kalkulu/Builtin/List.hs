{-# LANGUAGE OverloadedLists #-}

module Kalkulu.Builtin.List (listBuiltins) where

import qualified Data.Vector as V
import Kalkulu.Builtin
import qualified Kalkulu.BuiltinSymbol as B

listBuiltins :: [(B.BuiltinSymbol, BuiltinDefinition)]
listBuiltins = [
    (B.List, list)
  , (B.ListQ, listQ)
  , (B.Length, length_)
  , (B.All, all_)
  , (B.None, none)
  -- , (B.Span, span)
  -- , (B.Level, level)
  , (B.Head, head_)
  , (B.Part, part)
  -- , (B.Partition, partition)
  -- , (B.Extract, extract)
  , (B.Flatten, flatten)
  -- , (B.First, first_)
  -- , (B.Last, last_)
  -- , (B.Most, most)
  -- , (B.Rest, rest)
  -- , (B.ReplacePart, replacePart)
  -- , (B.Take, take_)
  -- , (B.Drop, drop_)
  -- , (B.Select, select)
  -- , (B.Split, split_)
  -- , (B.SplitBy, splitBy_)
  -- , (B.Cases, cases)
  -- , (B.DeleteCases, deleteCases)
  -- , (B.Position, position)
  -- , (B.MemberQ, memberQ)
  -- , (B.ConstantArray, constantArray)
  -- , (B.Array, array_)
  -- , (B.Table, table)
  -- , (B.Join, join)
  -- , (B.Catenate, catenate)
  -- , (B.Append, append)
  -- , (B.AppendTo, appendTo)
  -- , (B.Prepend, prepend)
  -- , (B.PrependTo, prependTo)
  -- , (B.Tuples, tuples)
  -- , (B.Reap, reap)
  -- , (B.Sow, sow)
  -- , (B.UnitVector, unitVector)
  -- , (B.Riffle, riffle)
  -- , (B.Gather, gather)
  -- , (B.GatherBy, gatherBy)
  -- , (B.Tally, tally)
  -- , (B.DeleteDuplicates, deleteDuplicates)
  -- , (B.Union, union)
  -- , (B.Intersection, intersection)
  -- , (B.Complement, complement)
  -- , (B.IntersectingQ, intersectingQ)
  -- , (B.DisjointQ, disjointQ)
  -- , (B.Fold, fold)
  -- , (B.FoldList, foldList)
  -- , (B.Accumulate, accumulate)
  -- , (B.Total, total)
  -- , (B.Reverse, reverse)
  -- , (B.Mean, mean)
  -- , (B.Variance, variance)
  -- , (B.StandardVariation, standardVariation)
  -- , (B.RotateLeft, rotateLeft)
  -- , (B.RotateRight, rotateRight)
  -- , (B.Median, median)
  -- , (B.RankedMin, rankedMin)
  -- , (B.RankedMax, rankedMax)
  -- , (B.TakeLargest, takeLargest)
  -- , (B.TakeLargestBy, takeLargestBy)
  -- , (B.TakeSmallest, takeSmallest)
  -- , (B.TakeSmallestBy, takeSmallestBy)
  ]

list :: BuiltinDefinition
list = defaultBuiltin {
  attributes = [Locked, Protected]
  }

listQ :: BuiltinDefinition
listQ = defaultBuiltin {
  downcode = return . toExpression . pureListQ -- TODO: oneArg
  }

pureListQ :: Expression -> Bool
pureListQ (Cmp _ [CmpB B.List _]) = True
pureListQ _                       = False

length_ :: BuiltinDefinition
length_ = defaultBuiltin {
  downcode   = return . pureLength -- TODO: one arg
  }

pureLength :: Expression -> Expression
pureLength (Cmp _ [Cmp _ args]) = toExpression (length args)
pureLength (Cmp _ [_])          = toExpression (0 :: Integer)
pureLength e                    = e

all_ :: BuiltinDefinition
all_ = defaultBuiltin

none :: BuiltinDefinition
none = defaultBuiltin

head_ :: BuiltinDefinition
head_  = defaultBuiltin {
  downcode = return . pureHead -- TODO: one arg
  }

pureHead :: Expression -> Expression
pureHead (Cmp _ [e]) = getHead e
pureHead e = e

part :: BuiltinDefinition
part = defaultBuiltin {
    attributes = [NHoldRest, Protected]
  , downcode = downcodePart
  }

downcodePart :: Expression -> Kernel Expression
downcodePart (Cmp _ [e, Number 0]) = return $ getHead e
downcodePart e@(Cmp _ [Cmp _ args, Number i]) =
  let i' = fromInteger i in
  let j = if i' > 0 then i'-1 else length args + i' in
  case args V.!? j of
    Nothing -> return e -- TODO: sendMessage
    Just a  -> return a

flatten :: BuiltinDefinition
flatten = defaultBuiltin {
  downcode   = downcodeFlatten -- TODO: oneArg
  }

downcodeFlatten :: Expression -> Kernel Expression
downcodeFlatten (Cmp _ [Cmp h args]) = return $ Cmp h (flattenHead h args)
downcodeFlatten e@(Cmp _ [_]) = return e -- sendMessage
downcodeFlatten e = return e

flattenHead :: Expression -> V.Vector Expression -> V.Vector Expression
flattenHead h = V.concatMap (help h)
  where help h e@(Cmp h' as)
          | h == h'   = flattenHead h as
        help _ e = V.singleton e
