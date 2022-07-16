module Assignments.Pets (result, i'mReadyForNextTask, makePetFeelGood) where

import Pets (
  -- Pets
    randomPet
  , poorAqua
  , fatJohn
  , Pet
  -- Manipulations
  , giveWater
  , giveRest
  , feed
  -- Checking state
   -- globally
  , condition
  , feelsGood
   -- by exact parameters
  , wannaEat
  , wannaDrink
  , wannaRest
  , overeaten
  , overdrank
  , overslept
  -- Things
  , FoodSort (Meat,Vegetables)
  )


import Prelude (Int, Num((+)), negate, ($), (.), print, error, Bool (True,False),Ord((>),(<),(>=),(<=)), undefined, Integer, (*), (==))
import APrelude (if_then_else)

-- * How to check your results:
-- Type `cabal run` in the terminal (Ctrl + Shift + ~)

-- * First assignment
-- is to make those pets feels good
result :: (Pet, Pet)
result = (happyJohn,happyAqua)

-- >>> wannaRest (fatJohn)
-- False

fatJohn2 = giveRest -4 fatJohn
-- >>> condition fatJohn2
-- fromList []
-- >>> feelsGood fatJohn2
-- True

happyAqua :: Pet
happyAqua = feed Meat 4 (giveRest 1  (giveWater 1 poorAqua))

happyJohn :: Pet
happyJohn = fatJohn2

-- * Next assignment
-- When you're done with first task, go do next one:
i'mReadyForNextTask :: Bool
i'mReadyForNextTask = True -- set it to 'True'

makePetFeelGood :: Pet -> Pet



makePetFeelGood = \bePolist -> if_then_else (feelsGood bePolist) bePolist (f bePolist)




-- * Worksheet:

-- Food
_ = Meat :: FoodSort
_ = Vegetables :: FoodSort

-- Manipulations
{-  Note, that giving a negative amout of `x`
    means that you prevent pet from getting 
    this amount of `x` for some period of time
-}
_ = giveWater
_ = giveRest
_ = feed

-- Checking state
_ = condition
_ = feelsGood

_ = wannaEat
_ = wannaDrink
_ = wannaRest
_ = overeaten
_ = overdrank
_ = overslept

-- Make them feel themselves good:
_ = poorAqua
_ = fatJohn

-- Examples of how to use functions

_ = negate

is'2'greater'than'9 :: Bool
is'2'greater'than'9 = 2 > 9

-- | This is a function of two integer numbers, that returns greatest of those
max :: Int -> Int -> Int
max = \a b -> if_then_else (a > b) a b 
  -- "if a greater than b, then return a, else return b"

have :: Int -> Int 
have = \s -> if_then_else ( s > 0) s (negate s)


-- >>> have 9
-- 9

-- | This is a function of one argument. It takes an Int and returns an Int
addFive :: Int -> Int
addFive = \n -> n + 5 -- lambda n. n + 5


