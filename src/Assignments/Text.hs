{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}
module Assignments.Text where

import Prelude
  ( Int
  , Num((+))
  , ($)
  , (.)
  , print
  , error
  , Bool (True,False)
  , Ord((>),(<),(>=),(<=))
  , undefined
  , Integer
  , (*)
  , (==)
  , (++)
  , (^)
  , String, flip, filter, map, Char, Foldable (length)
  )
import Data.Char (isUpper)
import Text (firstSatisfies)

-- a -> b -> c = a -> (b -> c)
-- a b c = ((a b) c)

concat :: String -> String -> String
concat = (++)

-- | Восклицание
exclaim :: String -> String
exclaim = \s -> concat s "!"

-- | Поприветствовать
greet :: String -> String
greet = \name -> concat "Hi, my dear friend " name

bob :: String
bob = "Bob"

-- To do:
_ = "Hi, my dear friend Bob"
_ = "Hi, my dear friend Bob. Ignat have married Sergey?"
  -- actually, "Have Ignat married Sergey" would be more correct but...

-- | Поженились
marry :: String -> (String -> String)
marry = \one second -> concat one (concat " have married " second)

-- | Спросить
ask :: String -> String
ask = \question -> concat question "?"

finish :: String -> String
finish = flip concat "." 

space :: String -> String -> String
space = \a -> concat (concat a " ")

-- * Assignment 2a
{-
byTemplate "Bob" = "Hi, my dear friend Bob. Ignat have married Sergey?"
byTemplate "Gregory" = "Hi, my dear friend Gregory. Ignat have married Sergey?"
-}

byTemplate :: String -> String
byTemplate = \name ->
  space (finish (greet name)) (ask (marry "Ignat" "Sergey"))

{-
byTemplate2 "Bob" "Charlie" "George" = "Hi, my dear friend Bob. Charlie have married George?"
byTemplate2 "Gregory" "Eelon Musk" "you" = "Hi, my dear friend Gregory. Eelon Musk have married you?"
-}

byTemplate2 :: String -> String -> String
byTemplate2 = undefined

-- * Assignment 3
{-
Написякай функцию, которая берет на вход список имен и возвращает список фраз
"by template" для тех имен, которые начинаются с большой буквы.

phrasesByTemplate ["Vitya","tanya","Cook"] =
  [ "Hi, my dear friend Vitya. Ignat have married Sergey?"
  , "Hi, my dear friend Cook. Ignat have married Sergey?" ]
-}

-- | является ли символ (Char) прописной буквой
_ = isUpper :: Char -> Bool 
_ = filter :: forall a. (a -> Bool) -> [a] -> [a]
_ = map :: forall a b. (a -> b) -> [a] -> [b]
-- | удовлетворяет ли первый элемент списка условию
_ = firstSatisfies :: forall a. (a -> Bool) -> [a] -> Bool 
-- | оператор композиции функций. `(f . g) x = f (g x)`
_ = (.) :: forall a b c. (b -> c) -> (a -> b) -> a -> c

phrasesByTemplate :: [String] -> [String]
phrasesByTemplate = map byTemplate . filter (firstSatisfies isUpper)

-- * Assignment 4
{-
Напиши функцию weirdTemplate, которая:
- принимает список имён
- добавляет приставку "Sir" ("Ivan" |-> "Sir Ivan") ко всем именам
- возвращает список всех получившихся фраз для этих имен, которые короче 49 символов
-}
_ = length :: forall a. [a] -> Int

dontTouch1 :: forall a. Ord a => a -> a -> Bool
dontTouch1 = (<)
dontTouch2 :: forall a. Ord a => a -> a -> Bool
dontTouch2 = (<=)
dontTouch3 :: forall a. Ord a => a -> a -> Bool
dontTouch3 = (>)
dontTouch4 :: forall a. Ord a => a -> a -> Bool
dontTouch4 = (>=)
dontTouch5 :: forall a. Ord a => a -> a -> Bool
dontTouch5 = (==)

weirdTemplate :: [String] -> [String]
weirdTemplate = undefined
