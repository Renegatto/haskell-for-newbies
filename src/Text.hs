{-# LANGUAGE ImportQualifiedPost #-}
module Text (test2a, test2b, test3, test4, firstSatisfies) where
import System.Random (randomIO)
import Control.Monad (replicateM)
import Data.Foldable (fold)
import Data.Char (isUpper)

correctTemplate :: String -> String -> String -> String
correctTemplate a b c = fold
  [ "Hi, my dear friend "
  , a, ". ", b, " have married ", c, "?"
  ]

correctTemplate' :: String -> String
correctTemplate' s = correctTemplate s "Ignat" "Sergey"

prop_2aCompleted :: (String -> String) -> String -> Bool
prop_2aCompleted f s = f s == correctTemplate' s

prop_2bCompleted ::
  (String -> String -> String -> String) ->
  String -> String -> String -> Bool
prop_2bCompleted f a b c = f a b c == correctTemplate a b c

prop_3Completed :: ([String] -> [String]) -> [String] -> Bool
prop_3Completed f as =
  f as == fmap correctTemplate' (filter (firstSatisfies isUpper) as)

prop_4Completed :: ([String] -> [String]) -> [String] -> Bool
prop_4Completed f as =
  (f as ==) 
  $ filter ((<= 53) . length)
  $ fmap (correctTemplate' . mappend "Sir ") as

test2a :: (String -> String) -> IO Bool
test2a f = prop_2aCompleted f <$> randomString

test2b :: (String -> String -> String -> String) -> IO Bool
test2b f = prop_2bCompleted f
  <$> randomString
  <*> randomString
  <*> randomString

test3 :: ([String] -> [String]) -> IO Bool
test3 f = prop_3Completed f <$> randomStrings

test4 :: ([String] -> [String]) -> IO Bool
test4 f = prop_4Completed f <$> randomStrings

randomStrings :: IO [String]
randomStrings = (`replicateM` randomString) =<< randomIO

randomString :: IO String
randomString = do
  len <- randomIO
  replicateM len randomIO

overFirst :: (a -> a) -> [a] -> [a]
overFirst _ [] = []
overFirst f (x:xs) = f x : xs

firstSatisfies :: (a -> Bool) -> [a] -> Bool
firstSatisfies _ [] = False
firstSatisfies f (x:_) = f x