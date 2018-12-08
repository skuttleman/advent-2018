module Day2 (step1, step2) where
import Data.Function
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Similar as Sim


step1 :: [String] -> String
step1 lines =
  lines
    & fmap (hasTwoOrThree . valSet . freq Map.empty)
    & foldl foldTogether ( 0, 0 )
    & multiply
    & show


step2 :: [String] -> String
step2 lines =
  lines
    & fmap Sim.fromString
    & findSimilar Set.empty


freq :: Map.Map Char Int -> String -> Map.Map Char Int
freq map [] = map
freq map (letter:s) =
  let
    map' =
      if (Map.member letter map) then
        Map.adjust (+ 1) letter map
      else
        Map.insert letter 1 map
  in
    freq map' s


valSet :: (Ord a, Ord b) => Map.Map b a -> Set.Set a
valSet = foldr Set.insert Set.empty . fmap snd . Map.toList


hasTwoOrThree :: Set.Set Int -> ( Bool, Bool )
hasTwoOrThree line = ( Set.member 2 line, Set.member 3 line )


addTrue :: Int -> Bool -> Int
addTrue num False = num
addTrue num True = num + 1


foldTogether :: ( Int, Int ) -> ( Bool, Bool ) -> ( Int, Int )
foldTogether ( twos, threes ) ( hasTwo, hasThree ) =
  ( addTrue twos hasTwo, addTrue threes hasThree )


multiply :: ( Int, Int ) -> Int
multiply ( a, b ) = a * b


matchingLetters :: String -> String -> String -> String
matchingLetters matches [] _ = matches
matchingLetters matches _ [] = matches
matchingLetters matches (l1:s1) (l2:s2) =
  matchingLetters (if l1 == l2 then matches ++ [l1] else matches) s1 s2


findSimilar :: Set.Set Sim.Similar -> [Sim.Similar] -> String
findSimilar set [] = error "No similar match found"
findSimilar set (s:more) =
  case Set.lookupLE s set of
    Just val ->
      if compare val s == EQ then
        matchingLetters "" (Sim.getString val) (Sim.getString s)
      else
        findSimilar (Set.insert s set) more
    Nothing -> 
      findSimilar (Set.insert s set) more
