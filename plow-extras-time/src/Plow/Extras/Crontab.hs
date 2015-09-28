module Plow.Extras.Crontab where

import           Data.Time
import           Data.Time.Calendar.WeekDate
import           Text.ParserCombinators.ReadP

data DOW = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Ord, Eq, Show, Bounded, Enum)

data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Ord, Eq, Show, Bounded, Enum)

type DOM = Int

type Hour = Int

type Minute = Int

data CronTabType = CTMinute Minute | CTHour Hour | CTDate DOM | CTMonth Month | CTDay DOW

data CronTab = CronTab { minute     :: Minute
                       , hour       :: Hour
                       , dayOfMonth :: DOM
                       , month      :: Month
                       , dayOfWeek  :: DOW} deriving (Eq, Ord, Show)


------------------------------------------------PARSING----------------------------------------------


runReadP :: ReadP b -> String -> [b]
runReadP incomingReadP  = fmap fst . readP_to_S incomingReadP

--converts UTCTime to a Crontab to be compared to parsed CronTab
utcToCronTab :: UTCTime -> CronTab
utcToCronTab time = CronTab m hr dom (toMonth mnth) (toDOW dow)
  where
    timeHMS = localTimeOfDay $ utcToLocalTime utc time
    (_, mnth, dom) = toGregorian $ utctDay time
    m = todMin timeHMS
    hr = todHour timeHMS
    (_, _, dow) = toWeekDate $ utctDay time

--checks equality of CronTab input and parsed CronTab
compareToParsedCron :: CronTab -> ReadP Bool
compareToParsedCron cron = do
  m <- parseRange parseMinute
  hr <- parseRange parseHour
  dom <- parseRange parseDOM
  mon <- parseRange parseMonth
  dow <- parseRange parseDOW
  return $ m (minute cron) && hr (hour cron) && dom (dayOfMonth cron) && mon (month cron) && dow (dayOfWeek cron)

parseRange :: (Enum a, Ord a) => ReadP a -> ReadP (a -> Bool)
parseRange p = parseAsterik <++ parseRng <++ parseList <++ parseSingleton
  where
    parseSingleton = do
      val <- p
      return $ (==) val
    parseList = do
      vals <- sepBy p (char ',')
      return $ flip elem vals
    parseRng = do
      first <- p
      _ <- char '-'
      lst <- p
      return $ flip elem [first .. lst]
    parseAsterik = do
      skipSpaces
      _ <- char '*'
      return $ const True
    -- Daniel: Unused code?
    --
    -- sort [] = []
    -- sort (x:xs) =
    --     let smallerSorted = sort (filter (<=x) xs)
    --         biggerSorted = sort (filter (>x) xs)
    --     in  smallerSorted ++ [x] ++ biggerSorted

parseMinute :: ReadP Minute
parseMinute = parseBoundedInt 0 59

parseHour :: ReadP Hour
parseHour = parseBoundedInt 0 23

parseDOM :: ReadP DOM
parseDOM = parseBoundedInt 1 31

parseBoundedInt :: Int -> Int -> ReadP Int
parseBoundedInt i f = parseInt >>= checkInt
  where
    checkInt i'
      |i <= i' && f >= i' = return i'
      |otherwise = fail "Integer out of range"
    parseInt =  readS_to_P reads :: ReadP Int

parseMonth :: ReadP Month
parseMonth = toMonth <$> readS_to_P reads

parseDOW :: ReadP DOW
parseDOW = do
  day <- readS_to_P reads :: ReadP Int
  return $ toDOW day

toMonth :: Int -> Month
toMonth monthInt =
  case monthInt of
      1 -> January
      2 -> February
      3 -> March
      4 -> April
      5 -> May
      6 -> June
      7 -> July
      8 -> August
      9 -> September
      10 -> October
      11 -> November
      12 -> December
      _ -> error $ "Invalid month: " ++ show monthInt

toDOW :: Int -> DOW
toDOW dayInt =
  case dayInt of
    0 -> Sunday
    1 -> Monday
    2 -> Tuesday
    3 -> Wednesday
    4 -> Thursday
    5 -> Friday
    6 -> Saturday
    _ -> error $ "Invalid Day of Week: " ++ show dayInt
