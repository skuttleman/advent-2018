module Day5 (step1, step2) where
import Data.Char
import Data.Function
import Data.List


step1 :: [String] -> String
step1 lines =
  lines
    & head
    & react
    & show


step2 :: [String] -> String
step2 lines =
  lines
    & head
    & withRemoved ['a'..'z']
    & fmap react
    & sort
    & head
    & show


react :: String -> Int
react = length . foldr react' ""


react' :: Char -> String -> String
react' x (y:ys)
  | isOpposite x y = ys
react' x ys = x:ys


isOpposite :: Char -> Char -> Bool
isOpposite letter1 letter2 =
  letter1 /= letter2 && toLower letter1 == toLower letter2


withRemoved :: [Char] -> String -> [String]
withRemoved letters polymer =
  letters
    & fmap (remove polymer)


remove :: String -> Char -> String
remove polymer letter =
  polymer
    & filter (\portion -> toLower portion /= letter)
