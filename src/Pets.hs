{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pets 
  ( poorAqua
  , fatJohn
  , condition
  , feelsGood
  , giveWater
  , giveRest
  , feed

  , wannaEat
  , wannaDrink
  , wannaRest
  , overeaten
  , overdrank
  , overslept
  , FoodSort(..)
  , Sort (..)
  -- For checking assignment
  , sort
  ) where
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeLits (TypeError(..), ErrorMessage (Text))


conditionIncludes :: Condition -> Pet -> Bool
conditionIncludes x pet = x ∈ condition pet 

overeaten :: Pet -> Bool
overeaten = conditionIncludes Oversatiety

overdrank :: Pet -> Bool
overdrank = conditionIncludes Hyperhydration

overslept :: Pet -> Bool
overslept = conditionIncludes Excitement

wannaEat :: Pet -> Bool
wannaEat = conditionIncludes Hunger

wannaDrink :: Pet -> Bool
wannaDrink = conditionIncludes Dehydration

wannaRest :: Pet -> Bool
wannaRest = conditionIncludes Sleepyness

conditionBySatiety :: Pet -> Set Condition
conditionByHydration :: Pet -> Set Condition
conditionByRest :: Pet -> Set Condition

-- | All negative effects that pet currently experience.
condition :: Pet -> Set Condition

-- | Gives pet certain amount (in `L`) of water
giveWater :: Float -> Pet -> Pet

-- | Feeds pet with certain type and amount (in `Kg`) of food
feed :: FoodSort -> Float -> Pet -> Pet

-- | @giveRest n pet@ gives `pet` to rest `n` hours.
giveRest :: Coeff -> Pet -> Pet

-- | How much `Kg` of food pet of this sort eats per day 
foodKgForSatiety :: forall p. Fractional p => Sort -> p

-- | Checks wheither pet feels good or not.
feelsGood :: Pet -> Bool
feelsGood = Set.null . condition

poorAqua:: Pet
poorAqua = Animal
  { name = "Aqua"
  , sort = Cat
  , thirst = 2
  , satiety = -3
  , rest = -0.3
  }

fatJohn :: Pet
fatJohn = Animal
  { name = "John"
  , sort = Dog
  , thirst = 0.02
  , satiety = 0.8
  , rest = 1.2
  }

data Sort
  = Dog
  | Cat
  | Worm
  | Rat
  | Hamster
  | Fish
  deriving (Show, Eq)

data Pet = Animal
  { name :: String
  , sort :: Sort
  , thirst :: Coeff
  , satiety :: Coeff
  , rest :: Coeff
  }

newtype Coeff = MkCoeff {getCoeff :: Float}
  deriving (Show, Num, Eq, Ord, Enum, Real, Fractional)

data Food = MkFood 
  { foodSort :: FoodSort
  , kg :: Float} deriving Show

data FoodSort
  = Meat
  | Vegetables
  | Fruits
  | Cereals
  deriving Show

data Condition
  = Dehydration
  | Hyperhydration
  | Weakness
  | Sleepyness
  | Excitement
  | Boredom
  | Hunger
  | Oversatiety
  deriving (Show,Eq,Ord)

conditionByRest Animal {rest} =
  if  | rest >= 1 -> [Boredom,Excitement]
      | rest < 0 -> [Sleepyness,Weakness]
      | otherwise -> []

conditionBySatiety Animal {satiety} =
  if  | satiety >= 1 -> [Oversatiety,Weakness]
      | satiety < 0 -> [Hunger,Weakness]
      | otherwise -> []


conditionByHydration Animal {thirst} =
  if  | thirst >= 1 -> [Dehydration,Weakness]
      | thirst < 0 -> [Hyperhydration,Weakness]
      | otherwise -> []

condition animal =
  foldMap @[] ($ animal)
    [ conditionByHydration
    , conditionBySatiety
    , conditionByRest ]

feed foodSort kg {-MkFood {foodSort,kg}-} pet@Animal {sort,satiety} =
  pet { satiety = satiety + satiety' }
  where 
    satiety' :: Coeff
    satiety' = (MkCoeff kg / foodKgForSatiety sort) *
      case foodSort of
        Meat -> case sort of
          Dog -> 1
          Cat -> 0.8
          Worm -> 0.7
          Rat -> 0.4
          Hamster -> 0
          Fish -> 0
        Vegetables -> case sort of
          Dog -> 0.2
          Cat -> 0.1
          Worm -> 0.9
          Rat -> 0.5
          Hamster -> 0.2
          Fish -> 0.3
        Fruits -> case sort of
          Dog -> 0.2
          Cat -> 0.1
          Worm -> 0.9
          Rat -> 0.5
          Hamster -> 0.2
          Fish -> 0.3
        Cereals -> case sort of
          Dog -> 0.2
          Cat -> 0.1
          Worm -> 0.9
          Rat -> 0.9
          Hamster -> 0.9
          Fish -> 0.7

petSizeCoeff :: Fractional p => Sort -> p
petSizeCoeff sort = case sort of
  Dog -> 1
  Cat -> 0.2
  Worm -> 0.01
  Rat -> 0.05
  Hamster -> 0.05
  Fish -> 0.04

foodKgForSatiety = (* 5) . petSizeCoeff

giveWater amount pet@Animal {thirst,sort} = pet { thirst = thirst - total }
  where 
    total = MkCoeff amount / (4 * petSizeCoeff sort)

giveRest hours pet@Animal {rest,sort} = pet { rest = rest + total }
  where 
    total = hours / (4 * petSizeCoeff sort) 

(∈) :: forall a. Ord a => a -> Set a -> Bool
(∈) = Set.member

instance TypeError ('Text "Can't show term of type `Pet`") => Show Pet
instance TypeError ('Text "Pet is not a number") => Num Pet
instance TypeError ('Text "Every Pet is unique and special!") => Eq Pet
instance TypeError ('Text "Every Pet is unique and special!") => Ord Pet