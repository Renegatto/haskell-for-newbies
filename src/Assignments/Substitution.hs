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

someBigExpression :: Int
someBigExpression = (5 * 7) + (3 - 12)

someBigExpression' :: Int
someBigExpression' = error "Not implemented yet :("