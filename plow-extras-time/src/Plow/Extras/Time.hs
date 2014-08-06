{-# LANGUAGE OverloadedStrings #-}
module Plow.Extras.Time where

import Data.Time
import Data.Time.Clock.POSIX

utcTimeToInt :: Integral c => UTCTime -> c
utcTimeToInt t = round.toRational $ utcTimeToPOSIXSeconds t


intToUTCTime :: Int -> UTCTime
intToUTCTime n = posixSecondsToUTCTime $ fromIntegral n 
