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
  , String, flip, filter, map, Char, Foldable (length), lookup
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

separator :: String -> String -> String -> String
separator = \sep x y -> concat x (concat sep y)

-- | Восклицание
exclaim :: String -> String
exclaim = \s -> concat s "!"

-- | Поприветствовать
greet :: String -> String
greet = \name -> concat "Привет, мой друг " name   

_ = "dfhd" -- строковый литерал
_ = 7878 -- числовой литерал

_ = "Привет, мой друг Ильюха"
ы = greet ильюха

_ = "Привет, мой друг Ильюха. Игнат женился на Сергее?"
ъ = space (greet (finish "Ильюха")) (ask(marry "Игнат" "Сергее"))


-- | Поженились
marry :: String -> String -> String
marry = separator " женился на "

-- | Спросить
ask :: String -> String
ask = \question -> concat question "?"

finish :: String -> String
finish = flip concat "." 

space :: String -> String -> String
space = separator " "

странныйВопрос = "А ты знала, что Евгений занимается евгеникой?"

-- * Assignment 2
{-
поШаблону "Полина" = "Привет, мой друг Полина. Игнат женился на Сергее?"
поШаблону "Витя" = "Привет, мой друг Витя. Игнат женился на Сергее?"

2a

поШаблону "Полина" "Петух" = "Привет, мой друг Полина. Петух женился на Сергее?"
поШаблону "Витя" "Конченый" = "Привет, мой друг Витя. Конченый женился на Сергее?"
-}

задание2готово :: Bool
задание2готово = False

поШаблону :: String -> String -> String
поШаблону = \name pups -> space (greet (finish name)) (ask(marry pups "Сергее"))

-----------------

choice :: String -> String -> String
choice = separator " или "

naOlge = "на Ольге"

ильюха = "Ильюха"

телятине = "телятине"


-- должно быть так
_ = "Ильюха женился на телятине или на Ольге?"

решение = 
  ask(choice(marry ильюха телятине) naOlge)






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
dontTouch5 :: forall a. Ord a => a -> a -> Bool
dontTouch5 = (==)

f :: [String] -> [String]
f = undefined