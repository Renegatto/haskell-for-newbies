{-# LANGUAGE DeriveFoldable #-}
module Assignments.Rec where
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import APrelude (if_then_else)
import Prelude hiding (concat)



{-@ -- Assigment 1a

sumStuff [1,3,4,78,3] == 1 + 3 + 4 + 78 + 3
sumStuff [] = 0

@-}

sumStuff :: [Integer] -> Integer
sumStuff = foldl (+) 0

mulStuff :: [Integer] -> Integer
mulStuff = foldl (*) 1

-- >>> mulStuff [1,3,4,2,3]
-- 72


{-@ -- Assigment 1b

glue ["ab","cd","325235"] == concat (concat (concat "" "ab")  "cd") "325235" -- "abcd325235"
glue [] = ""
@-}

concat :: String -> String -> String
concat a b = a ++ b

glue :: [String] -> String
glue = foldl concat ""


-- >>> glue ["bleu","sky" , "very" ]

{-@
glueAny [['a','b'],"cd","325235"] == "abcd325235" -- [[Char]] -> [Char]
glueAny [[True],[True,False],[],[False]] == [True,True,False,False] -- [[Bool]] -> [Bool]
@-}

q = [ 'a', 'b', 'c']

-- >>> glueAny [['a','b'],"cd","325235"]
-- "abcd325235"

glueAny :: forall a. [[a]] -> [a]
glueAny = foldl (++) []

-- Part two

{-@
-- This type constists a non empty list of elements
data NonEmpty a =
  a :| [a] -- this is how we construct `NonEmpty a`

4 :| [] :: NonEmpty Int -- the non empty list of single element `4`
2 :| [7,6,8] :: NonEmpty Int
"Hey " :| [" Bob,","how are ","ya"] :: NonEmpty String

@-}  

_ = foldl :: forall a b. (b -> a -> b) -> b -> [a] -> b
_ = foldl :: forall a b. (b -> a -> b) -> b -> NonEmpty a -> b

{-@ -- Assigment 2

maximum (7 :| [19,23,9,-6]) == 23

@-}

_ = NonEmpty.head :: forall a. NonEmpty a -> a

isGreaterThan :: Ord a => a -> a -> Bool
isGreaterThan a b = a > b

maximum' :: Ord a => NonEmpty a -> a
maximum' = \ass -> foldl max' (NonEmpty.head ass) ass 

-- >>> maximum' (7 NonEmpty.:| [109,23,912,-6])
-- 912

{-@ -- Assigment 2

maximumWithDefault 3 (7 :| [19,23,9,-6]) == 23
maximumWithDefault 800 (7 :| [19,23,9,-6]) == 800
maximumWithDefault 7 [19,23,9,-6] == 23
maximumWithDefault 7 (Just 10) == 10
maximumWithDefault 2 Nothing == 2
@-}

maximumWithDefault :: forall a f. (Ord a, Foldable f) => a -> f a -> a
maximumWithDefault = foldl max'

{-@
maximum = if_then_else _ _ _
-- значит, что мы что-то возвращаем функцию в зависимости от условия,
-- причем всегда одно и то же

foldl []
-- не имеет смысла, т.к. [] - не функция

NonEmpty.head _ _
-- не может быть, т.к.
  NonEmpty.head :: NonEmpty a -> a
-- а не
  NonEmpty.head :: _ -> _ -> _

NonEmpty.head (>)
-- не имеет смысла, т.к. 
  (>) :: Ord a => a -> a -> Bool
  -- а не
  (>) :: NonEmpty a
@-}

-- >>> error "boi" + 6
-- boi

-- эта функция - не пример, а инструмент
max' = \a b -> if_then_else (a > b) a b

-- ===============================
-- RECURSION
-- ===============================

{-@ -- Assigment 3
-- Make a factorial function

fac 6 = 1 * 2 * 3 * 4 * 5 * 6

@-}

fac :: Integer -> Integer
fac = \n -> if_then_else (n > 0) (n * fac (n-1)) 1

-- >>> fac 6
-- 720





{-@ -- Assignment 4

-- Given a predicate (< x), finds closest number (from given one) that satisfies it
smallerBy (< 5) 88 = 4
smallerBy (< 5) -45456 = -45456

@-}

smallerBy :: (Integer -> Bool) -> Integer -> Integer
smallerBy = \ p n -> if_then_else (p n) n (smallerBy p (n - 1)) 

-- >>> smallerBy (< 7) 90
-- 6


interval :: Double -> (Double -> Bool, Double -> Bool)
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

inInterval
  :: (Double -> Bool) -- ^ if given value is greater than desired one
  -> (Double -> Bool) -- ^ if given value is less than desired one
  -> (Double -> Bool) -- ^ if given value is exactly desired one
  -> Double
inInterval = undefined
