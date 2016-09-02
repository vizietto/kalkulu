{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Kalkulu.VectorPattern (pattern (:<),
                              pattern (:>)) where

import qualified Data.Vector as V

data ViewL a = EmptyL
             | NotEmptyL a (V.Vector a)

data ViewR a = EmptyR
             | NotEmptyR (V.Vector a) a

viewl :: V.Vector a -> ViewL a
viewl v | V.null v  = EmptyL
        | otherwise = NotEmptyL (V.head v) (V.tail v)

viewr :: V.Vector a -> ViewR a
viewr v | V.null v  = EmptyR
        | otherwise = NotEmptyR (V.init v) (V.last v)

infixr 7 :<
pattern (:<) x xs <- (viewl -> NotEmptyL x xs)
infixl 7 :>
pattern (:>) xs x <- (viewr -> NotEmptyR xs x)
