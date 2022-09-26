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

-- :seti -interactive-print=Text.Show.Unicode.uprint

-- a -> b -> c
-- a -> (b -> c)

-- a b c
-- ((a b) c)

concat :: String -> String -> String
concat = (++)

-- | Восклицание
exclaim :: String -> String
exclaim = \s -> concat s "!"

-- | Поприветствовать
greet :: String -> String
greet = \name -> concat "Привет, мой друг " name


поля = "Полина"

_ = "Привет, мой друг Полина"

_ = "Привет, мой друг Полина. Игнат женился на Сергее?"

-- | Поженились
marry :: String -> (String -> String)
marry = \one second -> concat one (concat " женился на " second)

-- | Спросить
ask :: String -> String
ask = \question -> concat question "?"

finish :: String -> String
finish = flip concat "." 

space :: String -> String -> String
space = \a -> concat (concat a " ")

странныйВопрос = "А ты знала, что Евгений занимается евгеникой?"

-- * Assignment 2
{-
поШаблону "Полина" = "Привет, мой друг Полина. Игнат женился на Сергее?"
поШаблону "Витя" = "Привет, мой друг Витя. Игнат женился на Сергее?"
-}

задание2готово :: Bool
задание2готово = False

поШаблону :: String -> String
поШаблону = undefined

-- * Assignment 3
{-
Написякай функцию, которая берет на вход список имен и возвращает список фраз
"по шаблону" для тех имен, которые начинаются с большой буквы.

фразыПоШаблону ["Витя","таня","Петя"] =
  [ "Привет, мой друг Витя. Игнат женился на Сергее?"
  , "Привет, мой друг Петя. Игнат женился на Сергее?" ]
-}
-- char 'D'
-- | является ли символ (Char) прописной буквой
_ = isUpper :: Char -> Bool 
_ = filter :: forall a. (a -> Bool) -> [a] -> [a]
_ = map :: forall a b. (a -> b) -> [a] -> [b]
-- | удовлетворяет ли первый элемент списка условию
_ = firstSatisfies :: forall a. (a -> Bool) -> [a] -> Bool 
-- | оператор композиции функций. `(f . g) x = f (g x)`
_ = (.) :: forall a b c. (b -> c) -> (a -> b) -> a -> c

фразыПоШаблону :: [String] -> [String]
фразыПоШаблону = undefined

-- * Assignment 4
{-
Напиши функцию f, которая:
- принимает список имён
- добавляет приставку "Сэр" ("Иван" |-> "Сэр Иван") ко всем именам
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

f :: [String] -> [String]
f = undefined