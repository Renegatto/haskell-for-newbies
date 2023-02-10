module Main where
import Assignments.Pets qualified as Pets
 (result, makePetFeelGood, i'mReadyForNextTask) 
import Pets qualified as PetsImpl
import Control.Monad (unless, replicateM, (>=>), when)
import Prelude
import qualified Assignments.Text as Text
import qualified Text as TestText

testPets :: Int -> IO Bool 
testPets amount = 
  all (PetsImpl.feelsGood . Pets.makePetFeelGood)
  <$> replicateM amount PetsImpl.randomPet

main :: IO ()
main = do
  putStrLn "yeah!"
  when Text.assignment2aIsReady $
    TestText.testText2 Text.byTemplate 30 >>= flip unless
    (error "Second Text task is not completed")
  let (john,aqua) = Pets.result
  when Pets.i'mReadyForNextTask $
    testPets 30 >>= flip unless
    (error "Your treatment doesn' work for some pets")
  unless (PetsImpl.sort aqua == PetsImpl.Cat) $
    error "Aqua is not a cat. What you did to her???"
  unless (PetsImpl.sort john == PetsImpl.Dog) $
    error "John is not a dog. Do you try to trick me???"
  unless (PetsImpl.feelsGood john) $
    error "John doesn't feel good. Try again."
  unless (PetsImpl.feelsGood aqua) $
    error "Auqua is not okay. Try again."