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
  , String, flip, filter, map
  )
import Data.Char (isUpper)
import Text (overFirst)

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
задание2готово = True

поШаблону :: String -> String
поШаблону = \name ->
  space (finish (greet name)) (ask (marry "Игнат" "Сергее")) 

-- * Assignment 3
{-
Написякай функцию, которая берет на вход список имен и возвращает список фраз
"по шаблону" для тех имен, которые начинаются с большой буквы.

фразыПоШаблону ["Витя","таня","Петя"] =
  [ "Привет, мой друг Витя. Игнат женился на Сергее?"
  , "Привет, мой друг Петя. Игнат женился на Сергее?" ]
-}
_ = isUpper -- Является ли символ (Char) прописной буквой
_ = filter
_ = map
_ = overFirst :: (a -> a) -> [a] -> [a] -- применяет функцию к первому элементу списка
_ = (.) -- оператор композиции функций. `(f . g) x = f (g x)`

фразыПоШаблону :: [String] -> [String]
фразыПоШаблону = undefined