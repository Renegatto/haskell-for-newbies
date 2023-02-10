module Main where
import Assignments.Pets qualified as Pets
import Pets qualified as PetsImpl
import Control.Monad (unless, replicateM, (>=>), when)
import Prelude
import qualified Assignments.Text as Text
import qualified Text as TestText
import Data.Maybe (fromMaybe)
import qualified Data as DataImpl
import qualified Assignments.Data as Data

testData :: IO ()
testData = do
  when Data.nextStepToCatchIsReady $
    check
      (pure DataImpl.test_nextStepToCatch)
      (failTest "Data nextStepToCatch" Nothing)

testText :: IO ()
testText = do
  when Text.assignment2aIsReady $
    check
      (repeatCheck 10 $ TestText.test2a Text.byTemplate)     
      (failTest "text 2a" Nothing)
  when Text.assignment2aIsReady $
    check
      (repeatCheck 10 $ TestText.test2b Text.byTemplate2)     
      (failTest "text 2b" Nothing)
  when Text.assignment3IsReady $
    check
      (repeatCheck 10 $ TestText.test3 Text.phrasesByTemplate)     
      (failTest "text 3" Nothing)
  when Text.assignment4IsReady $
    check
      (repeatCheck 10 $ TestText.test4 Text.weirdTemplate)     
      (failTest "text 4" Nothing)

testPets :: IO ()
testPets = do
  when Pets.i'mReadyForNextTask $
    testMakePetFeelGood 30 >>= flip unless
    (error "Your treatment doesn' work for some pets")
  when Pets.firstAssignmentIsReady $ do
    let (john,aqua) = Pets.result
    unless (PetsImpl.sort aqua == PetsImpl.Cat) $
      error "Aqua is not a cat. What you did to her???"
    unless (PetsImpl.sort john == PetsImpl.Dog) $
      error "John is not a dog. Do you try to trick me???"
    unless (PetsImpl.feelsGood john) $
      error "John doesn't feel good. Try again."
    unless (PetsImpl.feelsGood aqua) $
      error "Auqua is not okay. Try again."

testMakePetFeelGood :: Int -> IO Bool 
testMakePetFeelGood amount = 
  all (PetsImpl.feelsGood . Pets.makePetFeelGood)
  <$> replicateM amount PetsImpl.randomPet

main :: IO ()
main = do
  putStrLn "yeah!"
  testText
  testData
  testPets

-- tools for testing

failTest :: String -> Maybe String -> a
failTest testName details = error $
  "Your solution for assignment " ++ testName
  ++ " is incorrect. "
  ++ fromMaybe " Try figure out why!" details

check :: IO Bool -> IO () -> IO ()
check test err = test >>= flip unless err

repeatCheck :: Int -> IO Bool -> IO Bool
repeatCheck times cond =
  and <$> replicateM times cond