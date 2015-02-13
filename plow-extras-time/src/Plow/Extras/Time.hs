
{-# LANGUAGE OverloadedStrings #-}

module Plow.Extras.Time where

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
  ( utcTimeToPOSIXSeconds
  , posixSecondsToUTCTime
    )

-- | 'UTCTime' to Epoch Time transformation.
utcTimeToInt :: Integral c => UTCTime -> c
utcTimeToInt = round . toRational . utcTimeToPOSIXSeconds

-- | Epoch Time to 'UTCTime' transformation.
intToUTCTime :: Int -> UTCTime
intToUTCTime = posixSecondsToUTCTime . fromIntegral
