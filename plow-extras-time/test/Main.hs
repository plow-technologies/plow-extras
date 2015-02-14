
import Plow.Extras.Time

import Data.Time.Clock (diffUTCTime)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck.Instances ()

main :: IO ()
main = defaultMain $ testGroup "plow-extras-time"
  [ testProperty "intToUTCTime . utcTimeToInt ~= id" $
      \t -> abs (diffUTCTime (intToUTCTime $ utcTimeToInt t) t) <= 1
  , testProperty "utcTimeToInt . intToUTCTime = id" $
      \t -> (utcTimeToInt . intToUTCTime) t == t
    ]
