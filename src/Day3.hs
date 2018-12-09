module Day3 (step1, step2) where
import Data.Function
import Data.Vector2D
import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils.Regex


data Sheet = Sheet Int [Vector2D Int]


step1 :: [String] -> String
step1 lines =
  lines
    & fmap (positions . matchInfo)
    & concat
    & countOverlaps Set.empty Set.empty
    & show


step2 :: [String] -> String
step2 lines =
  lines
    & fmap matchInfo
    & foldl (findOverlapping False) ( Map.empty, Set.empty )
    & snd
    & Set.toList
    & head
    & show


matchInfo :: String -> Sheet
matchInfo str =
  str #= "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)"
    & fmap read
    & (\(id:x:y:w:h:_) -> Sheet id [vector2D x' y' | x' <- [x..(x + w - 1)], y' <- [y..(y + h - 1)]])


countOverlaps :: Set.Set (Vector2D Int) -> Set.Set (Vector2D Int) -> [Vector2D Int] -> Int
countOverlaps set1 set2 [] = Set.size set2
countOverlaps set1 set2 (position:more) =
  if Set.member position set1 then
    countOverlaps set1 (Set.insert position set2) more
  else
    countOverlaps (Set.insert position set1) set2 more


findOverlapping :: Bool -> ( Map.Map (Vector2D Int) Int, Set.Set Int ) -> Sheet -> ( Map.Map (Vector2D Int) Int, Set.Set Int )
findOverlapping True ( map, set ) (Sheet id []) = ( map, set )
findOverlapping False ( map, set ) (Sheet id []) = ( map, Set.insert id set )
findOverlapping overlaps ( map, set ) (Sheet id (position:positions)) =
  case Map.lookup position map of
    Just conflict -> findOverlapping True ( map, Set.delete conflict set ) (Sheet id positions)
    Nothing -> findOverlapping overlaps ( Map.insert position id map, set ) (Sheet id positions)


positions :: Sheet -> [Vector2D Int]
positions (Sheet _ positions) = positions
