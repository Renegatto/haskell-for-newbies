module Assignment (result) where

import Pets (
  -- Pets
    poorAqua
  , fatJohn
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

import Prelude (Int, Num((+)), negate, ($), (.), print, error, Bool,Ord((>),(<),(>=),(<=)))
import APrelude (if_then_else)

result = (happyJohn,happyAqua)

happyAqua = feed Meat 4 (giveRest 1 (giveWater 1 poorAqua))

happyJohn = error "Not implemented :("

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

-- | This is a function of one argument. It takes an Int and returns an Int
addFive :: Int -> Int
addFive = \n -> n + 5
