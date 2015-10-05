{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}

module Plow.Extras.Lens where



import Control.Category
import Data.Monoid

-- import Plow.Extras.Lens.Internal
import Data.Functor
import Control.Monad.Reader
import Control.Lens 
import Data.Foldable
import Prelude (Int)
-- |catPrisms l f  =  (some prism to a sumtype) -> (insertion funciton of the form (\a -> f monoid-container) ) -> container of only that sumtype constructor value 
-- > x = Set.fromList [Left 1 ,Right 3, Left 4]
-- > y = icatPrisms (_Just ) (Set.insert) x ==> [1,2,3]

#if MIN_VERSION_lens(4,13,0)
catPrisms :: forall r
                          a
                          (m :: * -> *)
                          (f :: * -> *)
                          a1
                          (p :: * -> * -> *)
                          a2.
                   (Foldable f, Monoid r, Monoid a2, MonadReader (f a1) m,
                    Indexable Int p) =>
                   ((a -> Const r a) -> p a1 (Const r a1)) -> (a -> a2 -> r) -> m r

#else
catPrisms :: (Functor (p a), Foldable f, Monoid r, Monoid a2,
                    MonadReader (f a1) m, Profunctor p,
                    Indexable Int p1) =>
                   (p a (Const r a) -> p1 a1 (Const r a1)) -> p a (a2 -> r) -> m r
             
#endif        
catPrisms l f = views (folded.l) (f ?? mempty)



-- | like catPrisms for indexed containers
-- > xmap =  Map.fromList [(2,Just "fours"),(1,Just "thres")]
-- > ymap =  icatPrisms (_Just) (Set.insert) x ==> fromList [(1,"thres"),(2,"fours")]


icatPrisms :: (Functor f, Monoid b, Monoid a, Indexable i1 p, FoldableWithIndex i1 f1,
                     MonadReader (f1 a2) f) =>
                    (Indexed i a1 (Const (a -> b) a1) -> p a2 (Const (a -> b) a2))
                    -> (i -> a1 -> a -> b) -> f b

icatPrisms l f = iviews (ifolded . l )  f ?? mempty




