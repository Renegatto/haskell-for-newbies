{-# LANGUAGE LambdaCase #-}
module Assignments.HomeAccounting (FundsSpend, main) where
import Text.Read (readMaybe)
import System.Environment (getArgs)
import Control.Monad (guard)

data FundsSpend

help :: String
help = "\
  \Usage: ( spent\n\
  \             --amount NUMBER\n\
  \             --currency (USD | BYN | EUR)\n\
  \             --date DD.MM.YYY[HH:MM]\n\
  \             --storage OUTPUT_FILE\n\
  \             --description TEXT\n\
  \       | help\n\
  \       )\
  \"

data Currency

storeSpendingEntry :: FilePath -> FundsSpend -> IO ()
storeSpendingEntry = undefined

main :: IO ()
main = do
  getArgs >>= \case
    ( "spent"
      :"--amount":rawAmount
      :"--currency":rawCurrency
      :"--date":rawDate
      :"--storage":outputFile
      :"--description":description
     ) -> do
        (  amount :: Float
         , currency :: Currency
         , date
         ) <- maybe (fail "Can't parse input parameters. Try --help for help") pure $ do
          amount <- readMaybe rawAmount
          currency <- readMaybe rawCurrency
          date <- parseDate rawDate
          pure (amount,currency,date)
        storeSpendingEntry outputFile _fundsSpend
    ["help"] -> putStrLn help
    _ -> fail $ "Cannot parse command.\n" <> help

parseDate :: String -> Maybe (Int,Int,Int,Maybe (Int,Int))
parseDate (d0:d1:'.':m0:m1:'.':y0:y1:y2:y3:hhmm) = do
  let
    dd = [d0,d1]
    mm = [m0,m1]
    yyyy = [y0,y1,y2,y3]
    isInBounds :: Int -> Int -> Int -> Bool
    isInBounds lower upper x = x <= upper && x >= lower
  day <- readMaybe dd
  month <- readMaybe mm
  year <- readMaybe yyyy
  time <- case hhmm of
    [] -> Just Nothing
    [h0,h1,':',mi0,mi1] -> do
      hours <- readMaybe [h0,h1]
      minutes <- readMaybe [mi0,mi1]
      guard $ isInBounds 0 24 hours
      guard $ isInBounds 0 60 minutes
      pure $ Just (hours,minutes)
    _ -> Nothing
  guard $ isInBounds 0 31 day
  guard $ isInBounds 0 12 month
  guard $ year >= 2024
  pure (day,month,year,time)
parseDate _ = Nothing
