module Day1
    ( step1, step2
    ) where
import Data.Function
import qualified Data.Set as Set


step1 :: [String] -> String
step1 lines =
  lines
    & fmap toInt
    & foldl (+) 0
    & show


step2 :: [String] -> String
step2 lines =
  lines
    & fmap toInt
    & cycle
    & findRepeated (Set.singleton 0) 0
    & show


toInt :: String -> Int
toInt s =
  if head s == '+' then
    read (drop 1 s)
  else
    read s


findRepeated :: Set.Set Int -> Int -> [Int] -> Int
findRepeated set freq (val:more) =
  let
    next = freq + val
  in
    if (Set.member next set) then
      next
    else
      findRepeated (Set.insert next set) next more
