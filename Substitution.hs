module Substitution where

e :: Int
e = 567

f :: Int -> Bool
f x = x > 0

f2 :: Int -> Float
f2 x = fromIntegral $ x + x * 2

f3 :: Int -> Integer
f3 x = fromIntegral $ x + 200


g :: Float -> Integer -> Int
g b x =
  (fromIntegral x + 4 :: Int) ^ round b

h :: Bool -> Int -> Float -> Double
h b x y =
  if b then
    fromIntegral $ x + x
  else
    fromIntegral $ round $ y * y