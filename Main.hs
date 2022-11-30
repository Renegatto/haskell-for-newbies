module Main where
import Assignments.Pets (result, makePetFeelGood, i'mReadyForNextTask)
import Pets (feelsGood, sort, Sort (Dog, Cat), {-randomPet-})
import Control.Monad (unless, replicateM, (>=>), when)
import Prelude
import qualified Assignments.Text as Text
import qualified Text as TestText
import Text (firstSatisfies)
import Data.Char (isUpper)

-- testPets :: Int -> IO Bool
-- testPets amount = 
--   all (feelsGood . makePetFeelGood)
--   <$> replicateM amount randomPet

main :: IO ()
main = do
  let correctImpl2b a b c = Text.ask $ Text.greet a `Text.space` (b `Text.marry` c)
      correctImpl2a a = correctImpl2b a "Ignat" "Sergey"
  when Text.assignment2a_isReady $ do
    let correct a = Text.byTemplate a == correctImpl2a a
    unless (all correct ["Bob","Gregory"]) $
      error "Assingemnt 2a failed"

  when Text.assignment2b_isReady $ do
    let examples =
          [("Misha","Bob","Penny")
          ,("Egor","Assault","Weapon")
          ,("Me","And","You")]
        correct (a,b,c) =
          Text.byTemplate2 a b c == correctImpl2b a b c 
    unless ( all correct examples) $
      error "Assingemnt 2b failed"

  when Text.assignment3_isReady $ do
    let examples =
          ["Misha","bob","Penny"
          ,"Egor","assault","Weapon"
          ,"Me","And","You"]
        correctImpl3 = map correctImpl2a . filter (firstSatisfies isUpper) 
        isCorrect =
          Text.phrasesByTemplate examples == correctImpl3 examples
    unless isCorrect $ error "Assingemnt 3 failed"

  when Text.assignment4_isReady $ do
    let examples =
          ["Misha","bob","Penny"
          ,"Egor","assault","Weapon"
          ,"Me","And","You"]
        correctImpl4 = filter ((53 >) . length) . map (correctImpl2a . mappend "Sir ")
        isCorrect =
          Text.weirdTemplate examples == correctImpl4 examples
    unless isCorrect $ error "Assingemnt 4 failed"


  --   TestText.testText2 Text.поШаблону 30 >>= flip unless
  --   (error "Second Text task is not completed")
  let (john,aqua) = result
  when i'mReadyForNextTask $ do
  --   testPets 30 >>= flip unless
  --   (error "Your treatment doesn' work for some pets")
    unless (sort aqua == Cat) $
      error "Aqua is not a cat. What you did to her???"
    unless (sort john == Dog) $
      error "John is not a dog. Do you try to trick me???"
    unless (feelsGood john) $
      error "John doesn't feel good. Try again."
    unless (feelsGood aqua) $
      error "Auqua is not okay. Try again."
  -- Text