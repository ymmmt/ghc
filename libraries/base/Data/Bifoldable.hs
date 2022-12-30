{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Bifoldable
  ( Bifoldable(..)
  , bifoldr'
  , bifoldr1
  , bifoldrM
  , bifoldl'
  , bifoldl1
  , bifoldlM
  , bitraverse_
  , bifor_
  , bimapM_
  , biforM_
  , bimsum
  , bisequenceA_
  , bisequence_
  , biasum
  , biList
  , binull
  , bilength
  , bielem
  , bimaximum
  , biminimum
  , bisum
  , biproduct
  , biconcat
  , biconcatMap
  , biand
  , bior
  , biany
  , biall
  , bimaximumBy
  , biminimumBy
  , binotElem
  , bifind
  ) where

import Control.Applicative
import Data.Functor.Utils (Max(..), Min(..), (#.))
import Data.Maybe (fromMaybe)
import Data.Monoid
import GHC.Generics (K1(..))
class Bifoldable p where
  {-# MINIMAL bifoldr | bifoldMap #-}

  bifold :: Monoid m => p m m -> m
  bifold = bifoldMap id id

  bifoldMap :: Monoid m => (a -> m) -> (b -> m) -> p a b -> m
  bifoldMap f g = bifoldr (mappend . f) (mappend . g) mempty

  bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bifoldr f g z t = appEndo (bifoldMap (Endo #. f) (Endo #. g) t) z

  bifoldl :: (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c
  bifoldl f g z t = appEndo (getDual (bifoldMap (Dual . Endo . flip f)
                                                (Dual . Endo . flip g) t)) z

instance Bifoldable (,) where
  bifoldMap f g ~(a, b) = f a `mappend` g b

instance Bifoldable Const where
  bifoldMap f _ (Const a) = f a

instance Bifoldable (K1 i) where
  bifoldMap f _ (K1 c) = f c

instance Bifoldable ((,,) x) where
  bifoldMap f g ~(_,a,b) = f a `mappend` g b

instance Bifoldable ((,,,) x y) where
  bifoldMap f g ~(_,_,a,b) = f a `mappend` g b

instance Bifoldable ((,,,,) x y z) where
  bifoldMap f g ~(_,_,_,a,b) = f a `mappend` g b

instance Bifoldable ((,,,,,) x y z w) where
  bifoldMap f g ~(_,_,_,_,a,b) = f a `mappend` g b

instance Bifoldable ((,,,,,,) x y z w v) where
  bifoldMap f g ~(_,_,_,_,_,a,b) = f a `mappend` g b

instance Bifoldable Either where
  bifoldMap f _ (Left a) = f a
  bifoldMap _ g (Right b) = g b

bifoldr' :: Bifoldable t => (a -> c -> c) -> (b -> c -> c) -> c -> t a b -> c
bifoldr' f g z0 xs = bifoldl f' g' id xs z0 where
  f' k x z = k $! f x z
  g' k x z = k $! g x z

bifoldr1 :: Bifoldable t => (a -> a -> a) -> t a a -> a
bifoldr1 f xs = fromMaybe (error "bifoldr1: empty structure")
                  (bifoldr mbf mbf Nothing xs)
  where
    mbf x m = Just (case m of
                      Nothing -> x
                      Just y  -> f x y)

bifoldrM :: (Bifoldable t, Monad m)
         => (a -> c -> m c) -> (b -> c -> m c) -> c -> t a b -> m c
bifoldrM f g z0 xs = bifoldl f' g' return xs z0 where
  f' k x z = f x z >>= k
  g' k x z = g x z >>= k

bifoldl':: Bifoldable t => (a -> b -> a) -> (a -> c -> a) -> a -> t b c -> a
bifoldl' f g z0 xs = bifoldr f' g' id xs z0 where
  f' x k z = k $! f z x
  g' x k z = k $! g z x

bifoldl1 :: Bifoldable t => (a -> a -> a) -> t a a -> a
bifoldl1 f xs = fromMaybe (error "bifoldl1: empty structure")
                  (bifoldl mbf mbf Nothing xs)
  where
    mbf m y = Just (case m of
                      Nothing -> y
                      Just x  -> f x y)

bifoldlM :: (Bifoldable t, Monad m)
         => (a -> b -> m a) -> (a -> c -> m a) -> a -> t b c -> m a
bifoldlM f g z0 xs = bifoldr f' g' return xs z0 where
  f' x k z = f z x >>= k
  g' x k z = g z x >>= k

bitraverse_ :: (Bifoldable t, Applicative f)
            => (a -> f c) -> (b -> f d) -> t a b -> f ()
bitraverse_ f g = bifoldr ((*>) . f) ((*>) . g) (pure ())

bifor_ :: (Bifoldable t, Applicative f)
       => t a b -> (a -> f c) -> (b -> f d) -> f ()
bifor_ t f g = bitraverse_ f g t

bimapM_ :: (Bifoldable t, Applicative f)
        => (a -> f c) -> (b -> f d) -> t a b -> f ()
bimapM_ = bitraverse_

biforM_ :: (Bifoldable t, Applicative f)
        => t a b ->  (a -> f c) -> (b -> f d) -> f ()
biforM_ = bifor_

bisequenceA_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequenceA_ = bisequence_

bisequence_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequence_ = bifoldr (*>) (*>) (pure ())

biasum :: (Bifoldable t, Alternative f) => t (f a) (f a) -> f a
biasum = bifoldr (<|>) (<|>) empty

bimsum :: (Bifoldable t, Alternative f) => t (f a) (f a) -> f a
bimsum = biasum

biList :: Bifoldable t => t a a -> [a]
biList = bifoldr (:) (:) []

binull :: Bifoldable t => t a b -> Bool
binull = bifoldr (\_ _ -> False) (\_ _ -> False) True

bilength :: Bifoldable t => t a b -> Int
bilength = bifoldl' (\c _ -> c+1) (\c _ -> c+1) 0

bielem :: (Bifoldable t, Eq a) => a -> t a a -> Bool
bielem x = biany (== x) (== x)

biconcat :: Bifoldable t => t [a] [a] -> [a]
biconcat = bifold

bimaximum :: forall t a. (Bifoldable t, Ord a) => t a a -> a
bimaximum = fromMaybe (error "bimaximum: empty structure") .
    getMax . bifoldMap mj mj
  where mj = Max #. (Just :: a -> Maybe a)

biminimum :: forall t a. (Bifoldable t, Ord a) => t a a -> a
biminimum = fromMaybe (error "biminimum: empty structure") .
    getMin . bifoldMap mj mj
  where mj = Min #. (Just :: a -> Maybe a)

bisum :: (Bifoldable t, Num a) => t a a -> a
bisum = getSum #. bifoldMap Sum Sum

biproduct :: (Bifoldable t, Num a) => t a a -> a
biproduct = getProduct #. bifoldMap Product Product

biconcatMap :: Bifoldable t => (a -> [c]) -> (b -> [c]) -> t a b -> [c]
biconcatMap = bifoldMap

biand :: Bifoldable t => t Bool Bool -> Bool
biand = getAll #. bifoldMap All All

bior :: Bifoldable t => t Bool Bool -> Bool
bior = getAny #. bifoldMap Any Any

biany :: Bifoldable t => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biany p q = getAny #. bifoldMap (Any . p) (Any . q)

biall :: Bifoldable t => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biall p q = getAll #. bifoldMap (All . p) (All . q)

bimaximumBy :: Bifoldable t => (a -> a -> Ordering) -> t a a -> a
bimaximumBy cmp = bifoldr1 max'
  where max' x y = case cmp x y of
                        GT -> x
                        _  -> y

biminimumBy :: Bifoldable t => (a -> a -> Ordering) -> t a a -> a
biminimumBy cmp = bifoldr1 min'
  where min' x y = case cmp x y of
                        GT -> y
                        _  -> x

binotElem :: (Bifoldable t, Eq a) => a -> t a a-> Bool
binotElem x =  not . bielem x

bifind :: Bifoldable t => (a -> Bool) -> t a a -> Maybe a
bifind p = getFirst . bifoldMap finder finder
  where finder x = First (if p x then Just x else Nothing)
