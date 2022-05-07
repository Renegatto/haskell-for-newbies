module Main where
import Assignment (result)
import Pets (feelsGood, sort, Sort (Dog, Cat))
import Control.Monad (unless)
import Prelude

main :: IO ()
main = do
  let (john,aqua) = result
  unless (sort aqua == Cat) $
    error "Aqua is not a cat. What you did to her???"
  unless (sort john == Dog) $
    error "John is not a dog. Do you try to trick me???"
  unless (feelsGood john) $
    error "John doesn't feel good. Try again."
  unless (feelsGood aqua) $
    error "Auqua is not okay. Try again."