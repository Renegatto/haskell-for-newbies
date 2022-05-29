module Assignments.Substitution where

import APrelude ()
import Substitution ( f, g, h, f2, f3, e )
import Prelude hiding (concat)
-- * Substitution
-- Fulfill `e'` the way it will be the same as `e`
-- But don't use names `c`, `b`, `a`

-- >>> c' == c
-- True

c' :: Double
c' = h (f (g(f2 e)(f3 e))) (g(f2 e)(f3 e)) (f2 (g(f2 e)(f3 e)))   

c :: Double
c = h (f b) a (f2 b)

b :: Int
b = g (f2 e) (f3 e)

a :: Int
a = g (f2 e) (f3 e)

-- * Operators to functions conversion
-- Express `someBigExpression` without use of * and +

add :: Int -> Int -> Int
add = \x y -> x + y

mul :: Int -> Int -> Int
mul = \x y -> x * y

sub :: Int -> Int -> Int
sub = \x y -> x - y

-- >>> someBigExpression == someBigExpression'
-- True

someBigExpression :: Int
someBigExpression = (5 * 7) + (3 - 12)

--someBigExpression' :: Int
someBigExpression' = add (mul 5 7) (sub 3 12)

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

-- >>> reverse "sdfdsfdsgsd"
-- "dsgsdfsdfds"

dsgsdfsdfds :: String
dsgsdfsdfds = "SUK"

sdfdsfdsgsd :: String
sdfdsfdsgsd = "ASS"

-- >>> dsgsdfsdfds' == dsgsdfsdfds
-- False

-- >>> dsgsdfsdfds'
-- "SSA"



-- >>> ['a','b','c'] == "abc"
-- True

-- >>> kon'2 == kon'
-- False

-- >>> kon'2
-- "Sela muha na gomno, tak i poletela"

kon'2 = concat a0 (concat (reverse a1) (reverse a2))


