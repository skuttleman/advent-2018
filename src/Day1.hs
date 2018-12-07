module Day1
    ( step1, step2
    ) where
import Data.Function

toInt :: String -> Int
toInt s =
  if head s == '+' then
    read (drop 1 s)
  else
    read s

step1 :: [String] -> String
step1 lines =
  lines
    & fmap toInt
    & foldl (+) 0
    & show

step2 :: [String] -> String
step2 _ = "tbd"
