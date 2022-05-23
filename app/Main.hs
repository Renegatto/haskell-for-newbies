module Main where
import Assignments.Pets (result, makePetFeelGood, i'mReadyForNextTask)
import Pets (feelsGood, sort, Sort (Dog, Cat), randomPet)
import Control.Monad (unless, replicateM, (>=>), when)
import Prelude

testPets :: Int -> IO Bool
testPets amount = 
  all (feelsGood . makePetFeelGood)
  <$> replicateM amount randomPet

main :: IO ()
main = do
  let (john,aqua) = result
  when i'mReadyForNextTask $
    testPets 30 >>= flip unless
    (error "Your treatment doesn' work for some pets")
  unless (sort aqua == Cat) $
    error "Aqua is not a cat. What you did to her???"
  unless (sort john == Dog) $
    error "John is not a dog. Do you try to trick me???"
  unless (feelsGood john) $
    error "John doesn't feel good. Try again."
  unless (feelsGood aqua) $
    error "Auqua is not okay. Try again."