import           Data.Time
import           Data.Time.Calendar           (Day)
import           Data.Time.Clock              (diffUTCTime)
import           Plow.Extras.Crontab
import           Plow.Extras.Time
import           Test.QuickCheck.Instances    ()
import           Test.Tasty                   (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck        (testProperty)
import           Text.ParserCombinators.ReadP

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [timeTests, cronParseTests, cronShouldSendTests]

timeTests = testGroup "plow-extras-time"
  [ testProperty "intToUTCTime . utcTimeToInt ~= id" $
      \t -> round (diffUTCTime (intToUTCTime $ utcTimeToInt t) t) == (0 :: Int)
  , testProperty "utcTimeToInt . intToUTCTime = id" $
      \t -> (utcTimeToInt . intToUTCTime) t == t
  ]

cronParseTests = testGroup "Crontab Parser"
  [ testCase "all asteriks 1" $
      testParser cron "* * * * *" @?= [(True,"")]
  , testCase "all ranges 1 - true" $
      testParser cron "0-59 0-23 1-31 1-12 0-6" @?= [(True,"")]
  , testCase "all ranges 1 - false" $
      testParser cron "1-59 1-23 2-31 2-12 2-6" @?= [(False,"")]
  , testCase "asteriks, ranges, and ints 1 - true" $
      testParser cron "* 0-23 * 1 0" @?= [(True,"")]
  , testCase "all lists 1 - true" $
      testParser cron "0,1,2,7 0,4,5,6 1,4 1,3 0,2" @?= [(True,"")]
  , testCase "all lists 1 - false" $
      testParser cron "1,2,7 0,4,5,6 1,4 1,3 0,2" @?= [(False,"")]
  , testCase "asteriks, ranges, ints, lists 1 - true" $
      testParser cron "* 0-6 1,4 1 0,2" @?= [(True,"")]
  , testCase "asteriks, ranges, ints, lists 1 - false" $
      testParser cron "0 1,4 1-4 * 0,2" @?= [(False,"")]
  , testCase "all lists 2 - true" $
      testParser cronTwo "0,1,2,5 0,4,5,6 1,4,13 1,3 0,2,3" @?= [(True,"")]
  , testCase "all lists 2 - false" $
      testParser cronTwo "0,1,2 0,4,5,6 4,13 1,3 0,2,3"  @?= [(False,"")]
  , testCase "asteriks, ranges, ints, lists 2 - true" $
      testParser cronTwo "* 0-6 1,4,13 3 0,2,3" @?= [(True,"")]
  , testCase "asteriks, ranges, ints, lists 2 - false" $
      testParser cronTwo "5 * 1-4 1,3 0,2" @?= [(False,"")]
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


testParser a = readP_to_S $ compareToParsedCron a

cron = CronTab 0 0 1 January Sunday
cronTwo = CronTab 5 4 13 March Wednesday

cronShouldSendTests = testGroup "Crontab shouldSend"
  [ testCase "all asteriks 1" $
      testShouldSend time "* * * * *" @?= [(True,"")]
  , testCase "all ranges 1 - true" $
       testShouldSend time "0-59 0-23 1-31 1-12 0-6" @?= [(True,"")]
  , testCase "all ranges 1 - false" $
      testShouldSend time "1-59 1-23 2-31 2-12 2-6" @?= [(False,"")]
  , testCase "asteriks, ranges, and ints 1 - true" $
     testShouldSend time "* 0-23 * 1 4" @?= [(True,"")]
  , testCase "asteriks, ranges, and ints 1 - false" $
      testShouldSend time "5-10 * * 4 2" @?= [(False,"")]
  , testCase "imposible range 1" $
      testShouldSend time "5-3 * * 4 2" @?= [(False,"")]
  , testCase "all asteriks 2" $
      testShouldSend timeTwo "* * * * *" @?= [(True,"")]
  , testCase "all ranges 2 - true" $
      testShouldSend timeTwo "0-10 2-9 13-30 1-12 0-6" @?= [(True,"")]
  , testCase "all ranges 2 - false" $
      testShouldSend timeTwo "1-10 1-23 2-31 5-12 2-6" @?= [(False,"")]
  , testCase "asteriks, ranges, and ints 2 - true" $
     testShouldSend timeTwo "* 0-23 16 10 *" @?= [(True,"")]
  , testCase "asteriks, ranges, and ints 2 - false" $
      testShouldSend timeTwo "5-10 * * 4 2" @?= [(False,"")]
  , testCase "imposible range 2" $
      testShouldSend timeTwo "5-10 * * 4-1 2" @?= [(False,"")]
  ]
  where
    shouldSend currentTime = compareToParsedCron $ utcToCronTab currentTime
    testShouldSend currentTime = readP_to_S $ shouldSend currentTime

time = UTCTime (ModifiedJulianDay 57023) 0 --January 1, 2015, 12:00 AM
timeTwo = UTCTime (ModifiedJulianDay 50372) 21600 --October 16, 1996, 6:00 AM


