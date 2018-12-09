module Day5 (step1, step2) where
import Data.Char
import Data.Function


step1 :: [String] -> String
step1 lines =
  lines
    & head
    & react
    & show


step2 :: [String] -> String
step2 _ = "tbd"


react :: String -> Int
react s =
  let
    len = length s
    next = react' s
    len' = length next
  in
    if len == len' then
      len
    else
      react next


react' :: String -> String
react' "" = ""
react' (letter:[]) = [letter]
react' (letter1:letter2:more) =
  if isOpposite letter1 letter2 then
    react' more
  else
    letter1:(react' (letter2:more))


isOpposite :: Char -> Char -> Bool
isOpposite letter1 letter2 =
  letter1 /= letter2 && toLower letter1 == toLower letter2
