{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
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
import Text.ParserCombinators.ReadP (string)

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

ignat = "Ignat"

sergey = "Sergey"

bob = "Bob"

-- To do:
_ = "Hi, my dear friend Bob"
_ = "Hi, my dear friend Bob. Ignat have married Sergey?"
  -- actually, "Have Ignat married Sergey" would be more correct but...

solution = space (greet (exclaim bob)) (ask (marry ignat sergey))

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

bob1 :: String -> String -> String
bob1 = \template -> undefined -- concat "Hi, my dear friend " template (template a "Bob") ". Ignat have married Sergey?"
-- * Assignment 2a
{-
byTemplate "Bob" = "Hi, my dear friend Bob. Ignat have married Sergey?"
byTemplate "Gregory" = "Hi, my dear friend Gregory. Ignat have married Sergey?"
-}

byTemplate :: String -> String
byTemplate = \k -> space (greet(exclaim k))(ask(marry ignat sergey))
q = "Illya"
{-@

byTemplate q
(\k -> space (greet(exclaim k))(ask(marry ignat sergey))) "Igor"
space (greet(exclaim "Igor"))(ask(marry ignat sergey))
@-}

{-
byTemplate2 "Bob" "Charlie" "George" = "Hi, my dear friend Bob. Charlie have married George?"
byTemplate2 "Gregory" "Eelon Musk" "you" = "Hi, my dear friend Gregory. Eelon Musk have married you?"
-}

byTemplate2 :: String -> String -> String -> String
byTemplate2 = \greg eelon you -> space (greet(exclaim greg))(ask(marry eelon you))

names = [byTemplate2]
_ = isUpper :: Char -> Bool 
_ = filter :: forall a. (a -> Bool) -> [a] -> [a]
_ = filter :: (String -> Bool) -> [String] -> [String]
_ = map :: forall a b. (a -> b) -> [a] -> [b]

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
_ = filter :: (String -> Bool) -> [String] -> [String]
_ = map :: forall a b. (a -> b) -> [a] -> [b]
-- | удовлетворяет ли первый элемент списка условию
_ = firstSatisfies :: forall a. (a -> Bool) -> [a] -> Bool 
-- | оператор композиции функций. `(f . g) x = f (g x)`
_ = (.) :: forall a b c. (b -> c) -> (a -> b) -> a -> c

phrasesByTemplate = \names ->
  map byTemplate (filter (firstSatisfies isUpper) names)

phrasesByTemplate1 :: [String] -> [String] 
phrasesByTemplate1 =
  map byTemplate . filter (firstSatisfies isUpper)
--phrase = 
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
weirdTemplate = filter smgg . map sirTemplate

sirTemplate :: String -> String 
sirTemplate = byTemplate . concat "Sir " 

smgg :: String -> Bool --Длина предложения
smgg =  (< 54) . length