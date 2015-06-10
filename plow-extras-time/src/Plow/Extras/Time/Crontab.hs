module Plow.Extras.Time.Crontab (shouldRun ) where

import           Data.Time.Calendar
import           Data.Time.Clock

--cronTabString >> "min(0-59) hour(0-23) dom(1-31) month(1-12) dow(0-6)"

splitBy delimiter = foldr f [[]]
            where f c l@(x:xs) | c == delimiter = []:l
                             | otherwise = (c:x):xs

--timeToNextRun :: String -> NominalDiffTime
--timeToNextRun =

shouldRun :: String -> UTCTime -> UTCTime -> Bool
shouldRun cronTabString currentTime lastRun  = undefined


