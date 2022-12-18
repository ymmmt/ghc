{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}

module Data.Bifunctor
  ( Bifunctor(..)
  ) where

import Control.Applicative  ( Const(..) )
import GHC.Generics ( K1(..) )

class (forall a. Functor (p a)) => Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

instance Bifunctor (,) where
    bimap f g ~(a, b) = (f a, g b)

instance Bifunctor ((,,) x1) where
    bimap f g ~(x1, a, b) = (x1, f a, g b)

instance Bifunctor ((,,,) x1 x2) where
    bimap f g ~(x1, x2, a, b) = (x1, x2, f a, g b)

instance Bifunctor ((,,,,) x1 x2 x3) where
    bimap f g ~(x1, x2, x3, a, b) = (x1, x2, x3, f a, g b)

instance Bifunctor ((,,,,,) x1 x2 x3 x4) where
    bimap f g ~(x1, x2, x3, x4, a, b) = (x1, x2, x3, x4, f a, g b)

instance Bifunctor ((,,,,,,) x1 x2 x3 x4 x5) where
    bimap f g ~(x1, x2, x3, x4, x5, a, b) = (x1, x2, x3, x4, x5, f a, g b)

instance Bifunctor Either where
    bimap f _ (Left a) = Left (f a)
    bimap _ g (Right b) = Right (g b)

instance Bifunctor Const where
    bimap f _ (Const a) = Const (f a)

instance Bifunctor (K1 i) where
    bimap f _ (K1 c) = K1 (f c)
