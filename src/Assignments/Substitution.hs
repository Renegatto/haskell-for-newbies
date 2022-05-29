module Assignments.Substitution where

import APrelude ()
import Substitution ( f, g, h, f2, f3, e )

-- * Substitution
-- Fulfill `e'` the way it will be the same as `e`
-- But don't use names `c`, `b`, `a`

c' :: Double
c' = error "Not implemented :("

c :: Double
c = h (f b) a (f2 b)

b :: Int
b = g (f2 e) (f3 e)

a :: Int
a = g (f2 a) (f3 a)

-- * Operators to functions conversion
-- Express `someBigExpression` without use of * and +

add :: Int -> Int -> Int
add = \x y -> x + y

mul :: Int -> Int -> Int
mul = \x y -> x * y

sub :: Int -> Int -> Int
sub = \x y -> x - y

someBigExpression :: Int
someBigExpression = (5 * 7) + (3 - 12)

-- * Strings concatenation

concat :: String -> String -> String
concat = \x y -> x <> y

mustBe :: String
mustBe = "Sela muha na gomno, tak i poletela"

a0 :: String
a0 = "Sela muha na gomno, "

a1 :: String
a1 = "tak i "

a2 :: String
a2 = "poletela"


resultOfConcat = error "Not implemented :("

-- * Operations over concatenated strings

_ = reverse

b0 :: String
b0 = "Sela muha na gomno, "

b1 :: String
b1 = reverse "tak i "

b2 :: String
b2 = reverse "poletela"

resultOfUnreversed = error "Not implemented :("

