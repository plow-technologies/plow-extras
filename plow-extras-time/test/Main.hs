import           Plow.Extras.Crontab
import           Plow.Extras.Time

import           Data.Time.Clock           (diffUTCTime)
import           Test.QuickCheck.Instances ()
import           Test.Tasty                (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck     (testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [timeTests, cronTests]

timeTests = testGroup "plow-extras-time"
  [ testProperty "intToUTCTime . utcTimeToInt ~= id" $
      \t -> round (diffUTCTime (intToUTCTime $ utcTimeToInt t) t) == (0 :: Int)
  , testProperty "utcTimeToInt . intToUTCTime = id" $
      \t -> (utcTimeToInt . intToUTCTime) t == t
  ]

cronTests = testGroup "plow-extras-crontab"
  [ testCase "all asteriks" $
      testParser cron "* * * * *" @?= [(True,"")]
  , testCase "all ranges - true" $
      testParser cron "0-59 0-23 1-31 1-12 0-6" @?= [(True,"")]
  , testCase "all ranges - false" $
      testParser cron "1-59 1-23 2-31 2-12 2-6" @?= [(False,"")]
  , testCase "asteriks, ranges, and ints - true" $
      testParser cron "* 0-23 * 1 0" @?= [(True,"")]
  , testCase "asteriks, ranges, and ints - false" $
      testParser cron "5-10 * * 4 2" @?= [(False,"")]
  , testCase "imposible range" $
      testParser cron "5-10 * * 4 2" @?= [(False,"")]
  ]
  where
    cron = CronTab 0 0 1 January Sunday
