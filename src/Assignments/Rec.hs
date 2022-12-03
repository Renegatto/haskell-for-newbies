module Assignments.Rec where
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty


_ = foldl :: forall a b. (b -> a -> b) -> b -> [a] -> b

{-@ -- Assigment 1a

sumStuff [1,3,4,78,3] == 1 + 3 + 4 + 78 + 3
sumStuff [] = 0

@-}

sumStuff :: [Integer] -> Integer
sumStuff = undefined

{-@ -- Assigment 1b

glue ["ab","cd","325235"] == "abcd325235"

@-}

glue :: [String] -> String
glue = undefined

-- Part two

{-@
-- This type constists a non empty list of elements
data NonEmpty a =
  a :| [a] -- this is how we construct `NonEmpty a`

4 :| [] :: NonEmpty Int -- the non empty list of single element `4`
2 :| [7,6,8] :: NonEmpty Int
"Hey " :| [" Bob,","how are ","ya"] :: NonEmpty String

@-}  


_ = foldl :: forall a b. (b -> a -> b) -> b -> NonEmpty a -> b

{-@ -- Assigment 2

maximum (7 :| [19,23,9,-6]) == 23

@-}

_ = NonEmpty.head :: forall a. NonEmpty a -> a

maximum :: Ord a => NonEmpty a -> a
maximum = undefined 

-- ===============================
-- RECURSION
-- ===============================

{-@ -- Assigment 3
-- Make a factorial function

fac 6 = 1 * 2 * 3 * 4 * 5 * 6

@-}

fac :: Integer -> Integer
fac = undefined

{-@ -- Assignment 4

-- Given a predicate (< x), finds closest number (from given one) that satisfies it
smallerBy (< 5) 88 = 4
smallerBy (< 5) -45456 = -45456

@-}

{-@ -- Assignment 5

-- Given a (satisfiable) predicate on a number, finds closest to 0 number that satisfies it
closestBy (< 5) = 4
closestBy (== 19) = 19
closestBy (>= -38) = -38
closestBy (\x -> x /= x) = -- never terminates

@-}

interval n = ((<= n), (>= n))

{-@ -- Assignment 5 (yeah boi!)

-- Given a pair of predicates,
-- where first tells if given number is less or equal to,
-- and second tells whether given number is greater or equal to,
-- finds a number that satisfies both (if any)

inInterval (interval 6) = 6
inInterval (interval 78) = 78
inInterval (interval -10) = -10

let x = inInterval (<= 10, >= 36)
in x <= 10, x >= 36

let x = inInterval (>= 36, <= 10)
in x <= 10, x >= 36

@-}

inInterval :: (Double -> Bool,Double -> Bool) -> Double
inInterval = undefined