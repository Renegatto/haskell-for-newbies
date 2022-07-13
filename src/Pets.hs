{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Pets 
  ( poorAqua
  , fatJohn
  , randomPet

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
  , Pet
  -- For checking assignment
  , sort
  ) where
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.TypeLits (TypeError, ErrorMessage (Text))
import System.Random (randomRIO, Random)
import Data.Functor ((<&>))
import Control.Monad (replicateM)
import Data.Kind (Type)

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

type Sort :: Type
data Sort
  = Dog
  | Cat
  | Worm
  | Rat
  | Hamster
  | Fish
  deriving stock (Show, Eq)

type Pet :: Type
data Pet = Animal
  { name :: String
  , sort :: Sort
  , thirst :: Coeff
  , satiety :: Coeff
  , rest :: Coeff
  }

type Coeff :: Type
newtype Coeff = MkCoeff Float
  deriving newtype (Show, Num, Eq, Ord, Enum, Real, Fractional)
  deriving newtype Random

randomCoeffR :: Float -> (Coeff,Coeff) -> IO (Coeff)
randomCoeffR c (MkCoeff mi,MkCoeff ma) = do
  isPos <- randomIO
  let signify = if isPos then 1 else (0-1)
  multiplier <- randomRIO (round $ mi / c :: Int, round $ ma / c) 
  pure $ signify * fromIntegral multiplier * MkCoeff c

type Food :: Type
data Food = MkFood 
  { foodSort :: FoodSort
  , kg :: Float} deriving stock Show

type FoodSort :: Type
data FoodSort
  = Meat
  | Vegetables
  | Fruits
  | Cereals
  deriving stock Show

type Condition :: Type
data Condition
  = Dehydration
  | Hyperhydration
  | Weakness
  | Sleepyness
  | Excitement
  | Boredom
  | Hunger
  | Oversatiety
  deriving stock (Show,Eq,Ord)

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

-- * Random pets

genPetSort :: IO Sort
genPetSort =  randomRIO (0 :: Int,6) <&>
  \case
    0 -> Dog
    1 -> Cat
    2 -> Worm
    3 -> Rat
    4 -> Hamster
    _ -> Fish

randomElem :: [a] -> IO a
randomElem xs = 
  (xs !!) <$> randomRIO (0,length xs - 1)

randomName :: IO String
randomName = do
  firstLetter <- randomElem ['A'..'Z']
  nameLength <- randomRIO (4,10)
  restOfName <- replicateM (pred nameLength) (randomElem ['a'..'z'])
  pure $ firstLetter : restOfName

randomPet :: IO Pet
randomPet = do
  name <- randomName
  sort <- genPetSort
  let sized :: forall m. Monad m => m Coeff -> m Coeff
      sized = fmap $ (*) (petSizeCoeff sort)
  thirst <- sized $ randomRIO (-2,2)
  satiety <- sized $ randomRIO (-5,3)
  rest <- sized $ randomRIO (-8,8)
  pure $ Animal { .. }