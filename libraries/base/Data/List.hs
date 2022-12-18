{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.List
   (

     (++)
   , head
   , last
   , tail
   , init
   , uncons
   , singleton
   , null
   , length

   , map
   , reverse

   , intersperse
   , intercalate
   , transpose

   , subsequences
   , permutations

   , foldl
   , foldl'
   , foldl1
   , foldl1'
   , foldr
   , foldr1

   , concat
   , concatMap
   , and
   , or
   , any
   , all
   , sum
   , product
   , maximum
   , minimum

   , scanl
   , scanl'
   , scanl1
   , scanr
   , scanr1

   , mapAccumL
   , mapAccumR

   , iterate
   , iterate'
   , repeat
   , replicate
   , cycle

   , unfoldr

   , take
   , drop
   , splitAt

   , takeWhile
   , dropWhile
   , dropWhileEnd
   , span
   , break

   , stripPrefix

   , group

   , inits
   , tails

   , isPrefixOf
   , isSuffixOf
   , isInfixOf
   , isSubsequenceOf

   , elem
   , notElem
   , lookup

   , find
   , filter
   , partition

   , (!!)

   , elemIndex
   , elemIndices

   , findIndex
   , findIndices

   , zip
   , zip3
   , zip4, zip5, zip6, zip7

   , zipWith
   , zipWith3
   , zipWith4, zipWith5, zipWith6, zipWith7

   , unzip
   , unzip3
   , unzip4, unzip5, unzip6, unzip7

   , lines
   , words
   , unlines
   , unwords

   , nub

   , delete
   , (\\)

   , union
   , intersect

   , sort
   , sortOn
   , insert

   , nubBy
   , deleteBy
   , deleteFirstsBy
   , unionBy
   , intersectBy
   , groupBy

   , sortBy
   , insertBy
   , maximumBy
   , minimumBy

   , genericLength
   , genericTake
   , genericDrop
   , genericSplitAt
   , genericIndex
   , genericReplicate

   ) where

import Data.Foldable
import Data.Traversable

import Data.OldList hiding ( all, and, any, concat, concatMap, elem, find,
                             foldl, foldl1, foldl', foldr, foldr1, mapAccumL,
                             mapAccumR, maximum, maximumBy, minimum, minimumBy,
                             length, notElem, null, or, product, sum )

import GHC.Base ( Bool(..), Eq((==)), otherwise )

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf []    _                    = True
isSubsequenceOf _     []                   = False
isSubsequenceOf a@(x:a') (y:b) | x == y    = isSubsequenceOf a' b
                               | otherwise = isSubsequenceOf a b
