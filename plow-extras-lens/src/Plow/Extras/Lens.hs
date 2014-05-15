{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}

module Plow.Extras.Lens where


import Control.Category
import Data.Monoid
import Plow.Extras.Lens.Internal

import Control.Lens 


-- |catPrisms l f  =  (some prism to a sumtype) -> (insertion funciton of the form (\a -> f monoid-container) ) -> container of only that sumtype constructor value 
-- > x = Set.fromList [Left 1 ,Right 3, Left 4]
-- > y = icatPrisms (_Just ) (Set.insert) x ==> [1,2,3]

catPrisms l f = views (folded.l) (f ?? mempty)



-- | like catPrisms for indexed containers
-- > xmap =  Map.fromList [(2,Just "fours"),(1,Just "thres")]
-- > ymap =  icatPrisms (_Just) (Set.insert) x ==> fromList [(1,"thres"),(2,"fours")]


icatPrisms l f = iviews (ifolded . l )  f ?? mempty



