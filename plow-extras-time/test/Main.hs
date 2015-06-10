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
  [ testCase "all asteriks 1" $
      testParser cron "* * * * *" @?= [(True,"")]
  , testCase "all ranges 1 - true" $
      testParser cron "0-59 0-23 1-31 1-12 0-6" @?= [(True,"")]
  , testCase "all ranges 1 - false" $
      testParser cron "1-59 1-23 2-31 2-12 2-6" @?= [(False,"")]
  , testCase "asteriks, ranges, and ints 1 - true" $
      testParser cron "* 0-23 * 1 0" @?= [(True,"")]
  , testCase "asteriks, ranges, and ints 1 - false" $
      testParser cron "5-10 * * 4 2" @?= [(False,"")]
  , testCase "imposible range 1" $
      testParser cron "5-3 * * 4 2" @?= [(False,"")]
  , testCase "all asteriks 2" $
      testParser cronTwo "* * * * *" @?= [(True,"")]
  , testCase "all ranges 2 - true" $
      testParser cronTwo "0-10 2-9 13-30 1-12 0-6" @?= [(True,"")]
  , testCase "all ranges 2 - false" $
      testParser cronTwo "1-10 1-23 2-31 5-12 2-6" @?= [(False,"")]
  , testCase "asteriks, ranges, and ints 2 - true" $
      testParser cronTwo "* 0-23 13 3 *" @?= [(True,"")]
  , testCase "asteriks, ranges, and ints 2 - false" $
      testParser cronTwo "5-10 * * 4 2" @?= [(False,"")]
  , testCase "imposible range 2" $
      testParser cronTwo "5-10 * * 4-1 2" @?= [(False,"")]
  ]
  where
    cron = CronTab 0 0 1 January Sunday
    cronTwo = CronTab 5 4 13 March Wednesday
