
-- | Types and functions that extend the 'base' package.
--
--   Please, do not add new dependencies without consent!
--
module Plow.Extras.Base (
    EitherPartition (..)
  , foldEitherPartition
  ) where

import Data.Monoid (Endo (..))

data EitherPartition err v = EitherPartition
  { eitherPartitionError :: err
  , eitherPartitionValue :: v
    }

foldEitherPartition
  :: Foldable f
  => (err -> err' -> err') -- ^ How to combine errors
  -> (v   -> v'   -> v'  ) -- ^ How to combine values
  -> EitherPartition err' v' -- ^ Initial partition
  -> f (Either err v)
  -> EitherPartition err' v'
foldEitherPartition errf vf p0 = flip appEndo p0 . foldMap (
  \e -> Endo $
    \p -> case e of
      Left err -> p { eitherPartitionError = errf err (eitherPartitionError p) }
      Right v  -> p { eitherPartitionValue = vf   v   (eitherPartitionValue p) }
  )
