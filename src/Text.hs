{-# LANGUAGE ImportQualifiedPost #-}
module Text (testText2) where
import System.Random (randomIO)
import Assignments.Text qualified as Text
import Control.Monad (replicateM)

prop_text2Completed :: String -> Bool
prop_text2Completed s =
   Text.поШаблону s ==
    "Привет, мой друг " ++ s ++ ". Игнат женился на Сергее?"

randomString :: IO String
randomString = do
  len <- randomIO
  replicateM len randomIO

testText2 :: Int -> IO Bool
testText2 amount =
  and <$> replicateM amount
    (prop_text2Completed <$> randomString)
