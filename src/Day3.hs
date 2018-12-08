module Day3
    ( step1, step2
    ) where
import Data.Function
import qualified Data.Set as Set
import Text.Regex.TDFA

matchInfo :: String -> ( Int, Int, Int, Int, Int )
matchInfo s =
  let
    (_, _, _, nums) = (s =~ "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)") :: ( String, String, String, [String] )
  in
    nums
      & fmap read
      & (\(id:x:y:w:h:_) -> ( id, x, y, w, h ))

expand :: ( Int, Int, Int, Int, Int ) -> ( Int, [( Int, Int )] )
expand (id, x, y, w, h) =
  ( id, [( x', y' ) | x' <- [x..(x + w - 1)], y' <- [y..(y + h - 1)]] )

countOverlaps :: Int -> Set.Set ( Int, Int ) -> Set.Set ( Int, Int ) -> [( Int, Int )] -> Int
countOverlaps counter set1 set2 [] = counter
countOverlaps counter set1 set2 (position:more) =
  if Set.member position set1 && Set.member position set2 then
    countOverlaps counter set1 set2 more
  else if Set.member position set1 then
    countOverlaps (counter + 1) set1 (Set.insert position set2) more
  else
    countOverlaps counter (Set.insert position set1) set2 more

step1 :: [String] -> String
step1 lines =
  lines
    & fmap (snd . expand . matchInfo)
    & concat
    & countOverlaps 0 Set.empty Set.empty
    & show

step2 :: [String] -> String
step2 _ = "tbd"
