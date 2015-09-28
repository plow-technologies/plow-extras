{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Plow.Extras.Time
Description :  Curently Plow.Extras.Time contains:
                        utcTimeToInt -- a function to convert utcTime to Int
                        intToUTCTime -- a function to convert int to UTCTime
                        diffTimeToInt -- a function to convert DiffTime to Int
                        intToDiffTime -- a function to convert Int to DiffTime

Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>
Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable
|-}

module Plow.Extras.Time where

import           Data.Time             (DiffTime, UTCTime, secondsToDiffTime)
import           Data.Time.Clock       (getCurrentTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime,
                                        utcTimeToPOSIXSeconds)

-- | 'UTCTime' to Epoch Time transformation.
utcTimeToInt :: Integral c => UTCTime -> c
utcTimeToInt = round . toRational . utcTimeToPOSIXSeconds

-- | Epoch Time to 'UTCTime' transformation.
intToUTCTime :: Int -> UTCTime
intToUTCTime = posixSecondsToUTCTime . fromIntegral

-- | 'DiffTime' to Int transformation.
diffTimeToInt :: Integral a => DiffTime -> a
diffTimeToInt = round . toRational

-- | Int (seconds) to DiffTIme transformation.
intToDiffTime :: Int -> DiffTime
intToDiffTime = secondsToDiffTime . fromIntegral

-- | Get the current epoch time in seconds.
getCurrentEpochTime :: IO Int
getCurrentEpochTime = utcTimeToInt <$> getCurrentTime
