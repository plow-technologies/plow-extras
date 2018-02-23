
{-# LANGUAGE CPP #-}

-- | Types and functions that extend the 'base' package.
--
--   Please, do not add new dependencies without consent!
--
module Plow.Extras.Base (
    -- * Either Partition
    EitherPartition (..)
  , foldEitherPartition
  , foldMapEitherPartition
  ) where

import Data.Monoid (Endo (..))
import Data.Bifunctor (Bifunctor (..))
#if MIN_VERSION_base(4,10,0)
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
#endif

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- EitherPartition

-- | Result of combining a collection of values of type 'Either'.
data EitherPartition err v = EitherPartition
  { eitherPartitionLeft  :: err
  , eitherPartitionRight :: v
    } deriving (Eq, Show)

-- | Fold a foldable value with values of type 'Either'.
foldEitherPartition
  :: Foldable f
  => (l -> l' -> l') -- ^ How to combine left values
  -> (r -> r' -> r') -- ^ How to combine right values
  -> EitherPartition l' r' -- ^ Initial partition
  -> f (Either l r)
  -> EitherPartition l' r'
foldEitherPartition lf rf p0 = flip appEndo p0 . foldMap (
  \e -> Endo $
    \p -> case e of
      Left  l -> p { eitherPartitionLeft  = lf l $ eitherPartitionLeft  p }
      Right r -> p { eitherPartitionRight = rf r $ eitherPartitionRight p }
  )

-- | Transform left and right values to monoid values, then combine the results.
foldMapEitherPartition
  :: (Foldable f, Monoid l', Monoid r')
  => (l -> l') -> (r -> r')
  -> f (Either l r)
  -> EitherPartition l' r'
foldMapEitherPartition lf rf = foldEitherPartition (mappend . lf) (mappend . rf) mempty

-- Instances

instance Functor (EitherPartition l) where
  fmap = second

instance Bifunctor EitherPartition where
  bimap  f g (EitherPartition l r) = EitherPartition (f l) (g r)
  first  f   (EitherPartition l r) = EitherPartition (f l)    r
  second f   (EitherPartition l r) = EitherPartition    l  (f r)

instance (Monoid l, Monoid r) => Monoid (EitherPartition l r) where
  mempty = EitherPartition mempty mempty
  mappend (EitherPartition l r) (EitherPartition l' r') =
    EitherPartition (mappend l l') (mappend r r')

instance Monoid l => Applicative (EitherPartition l) where
  pure = EitherPartition mempty
  EitherPartition l r <*> EitherPartition l' r' =
    EitherPartition (mappend l l') (r r')

instance Monoid l => Monad (EitherPartition l) where
  EitherPartition l r >>= f =
    let EitherPartition l' r' = f r
    in  EitherPartition (mappend l l') r'

#if MIN_VERSION_base(4,10,0)

instance Bifoldable EitherPartition where
  bifoldMap f g (EitherPartition l r) = mappend (f l) (g r)

instance Bitraversable EitherPartition where
  bitraverse f g (EitherPartition l r) = EitherPartition <$> f l <*> g r

#endif
