import           Plow.Extras.Time

import           Data.Time.Clock           (diffUTCTime)
import           Test.QuickCheck.Instances ()
import           Test.Tasty                (defaultMain, testGroup)
import           Test.Tasty.QuickCheck     (testProperty)

main :: IO ()
main = defaultMain $ testGroup "plow-extras-time"
  [ testProperty "intToUTCTime . utcTimeToInt ~= id" $
      \t -> round (diffUTCTime (intToUTCTime $ utcTimeToInt t) t) == (0 :: Int)
  , testProperty "utcTimeToInt . intToUTCTime = id" $
      \t -> (utcTimeToInt . intToUTCTime) t == t
    ]


