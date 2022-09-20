{-# LANGUAGE ImportQualifiedPost #-}
module Text (testText2,firstSatisfies) where
import System.Random (randomIO)
import Control.Monad (replicateM)

prop_text2Completed :: (String -> String) -> String -> Bool
prop_text2Completed f s =
   f s ==
    "Привет, мой друг " ++ s ++ ". Игнат женился на Сергее?"

randomString :: IO String
randomString = do
  len <- randomIO
  replicateM len randomIO

testText2 :: (String -> String) -> Int -> IO Bool
testText2 f amount =
  and <$> replicateM amount
    (prop_text2Completed f <$> randomString)

overFirst :: (a -> a) -> [a] -> [a]
overFirst _ [] = []
overFirst f (x:xs) = f x : xs

firstSatisfies :: (a -> Bool) -> [a] -> Bool
firstSatisfies _ [] = False
firstSatisfies f (x:_) = f x