module Assignments.Pets (result, i'mReadyForNextTask, makePetFeelGood) where

import Pets (
  -- Pets
  --  randomPet
    poorAqua
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


import Prelude (Int, Num((+)), negate, ($), (.), print, error, Bool (True,False),Ord((>),(<),(>=),(<=)), undefined)
import APrelude (if_then_else)

-- * How to check your results:
-- Type `cabal run` in the terminal (Ctrl + Shift + ~)

-- * First assignment
-- is to make those pets feels good
result :: (Pet, Pet)
result = (happyJohn,happyAqua)

happyAqua :: Pet
happyAqua = error "Happy Aqua is not implemented :("

happyJohn :: Pet
happyJohn = error "Happy John is not implemented :("

-- * Next assignment
-- When you're done with first task, go do next one:
i'mReadyForNextTask :: Bool
i'mReadyForNextTask = False -- set it to 'True'

makePetFeelGood :: Pet -> Pet
makePetFeelGood = error "Not implemented :(" -- implement this function

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

is'2'greater'than'9 :: Bool
is'2'greater'than'9 = 2 > 9

-- | This is a function of two integer numbers, that returns greatest of those
max :: Int -> Int -> Int
max = \a b -> if_then_else (a > b) a b 
  -- "if a greater than b, then return a, else return b"

-- | This is a function of one argument. It takes an Int and returns an Int
addFive :: Int -> Int
addFive = \n -> n + 5
