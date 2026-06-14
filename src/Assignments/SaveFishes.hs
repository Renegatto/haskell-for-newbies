module Assignments.SaveFishes where

import Data.List (partition)
import Prelude hiding (fmap, map)

data Fish id = FishInAquarium id | FishOutside id

-- >>> mySortBy (>=) [6,7,1,2,99]
-- [1,2,6,7,99]
someFishes = [FishOutside 1, FishOutside 2, FishOutside 3, FishOutside 4, FishOutside 5]

saveFish :: Fish a -> Fish a
saveFish (FishOutside x) = FishInAquarium x
saveFish fish = fish

saveFirstFish :: [Fish a] -> [Fish a]
saveFirstFish (firstFish : fishes) = saveFish firstFish : fishes
saveFirstFish [] = []

_ = (:)

-- saveFishes :: [Fish] -> [Fish]
-- saveFishes someFishes = saveFish someFishes
mySortBy :: (a -> a -> Bool) -> [a] -> [a]
mySortBy _ [] = []
mySortBy lessOrEqualTo (x : xs) =
  mySortBy lessOrEqualTo before
    ++ [x]
    ++ mySortBy lessOrEqualTo after
  where
    (before, after) = partition (lessOrEqualTo x) xs

take' :: Int -> [a] -> [a]
take' amount xs =
  foldr
    ( \x next -> \n ->
        if n == 0
          then []
          else case next (pred n) of
            xs' -> (x : xs')
    )
    (\_ -> [])
    xs
    amount

-- >>> take' 3 [0..]
-- [0,1,2]
