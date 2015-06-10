{-# LANGUAGE CPP,NoImplicitPrelude, NoMonomorphismRestriction, FlexibleContexts, TemplateHaskell #-}

module Plow.Extras.Lens where

import Prelude (($))
import Data.Char (toLower)
import Control.Category
import Data.Monoid
import Language.Haskell.TH
-- import Plow.Extras.Lens.Internal
import Data.Functor
import Control.Monad.Reader
import Control.Lens 
import Data.Foldable
import Prelude (Int)
-- |catPrisms l f  =  (some prism to a sumtype) -> (insertion funciton of the form (\a -> f monoid-container) ) -> container of only that sumtype constructor value 
-- > x = Set.fromList [Left 1 ,Right 3, Left 4]
-- > y = icatPrisms (_Just ) (Set.insert) x ==> [1,2,3]


catPrisms :: (Functor (p a), Foldable f, Monoid r, Monoid a2,
                    MonadReader (f a1) m, Profunctor p,
                    Indexable Int p1) =>
                   (p a (Const r a) -> p1 a1 (Const r a1)) -> p a (a2 -> r) -> m r

catPrisms l f = views (folded.l) (f ?? mempty)



-- | like catPrisms for indexed containers
-- > xmap =  Map.fromList [(2,Just "fours"),(1,Just "thres")]
-- > ymap =  icatPrisms (_Just) (Set.insert) x ==> fromList [(1,"thres"),(2,"fours")]


icatPrisms :: (Functor f, Monoid b, Monoid a, Indexable i1 p, FoldableWithIndex i1 f1,
                     MonadReader (f1 a2) f) =>
                    (Indexed i a1 (Const (a -> b) a1) -> p a2 (Const (a -> b) a2))
                    -> (i -> a1 -> a -> b) -> f b

icatPrisms l f = iviews (ifolded . l )  f ?? mempty




-- | Make lenses with underscore trailing for non-underscored records
-- non-classy variety without simple restrictions
#if !MIN_VERSION_lens (4,5,4)
makeLenses_ :: Name -> DecsQ
makeLenses_ t = makeLensesWith ?? t $ lensRules & lensField .~ lFcn
 where 
   lFcn _fieldNames n = case nameBase n of
                          x:xs -> [TopName (mkName ((toLower x:xs)  <> "_"))]
                          [] -> []

#else
makeLenses_ :: Name -> DecsQ
makeLenses_ t = makeLensesWith ?? t $ lensRules & lensField .~ lFcn
 where 
   lFcn _typeName _fieldNames n = case nameBase n of
                                    x:xs -> [TopName (mkName ((toLower x:xs)  <> "_"))]
                                    [] -> []
#endif
