module Day7 (step1, step2) where
import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils.Lists
import Utils.Maps
import Utils.Regex


step1 :: [String] -> String
step1 lines =
  lines
    & fmap dependency
    & foldl dependencyMap Map.empty
    & completeOrdered "" Set.empty
    & reverse


step2 :: [String] -> String
step2 _ = "tbd"

dependency :: String -> ( Char, Char )
dependency s =
  let
    (master:slave:_) = s #= "Step (.) must be finished before step (.) can begin."
  in
    ( head master, head slave )


dependencyMap :: Map.Map Char (Set.Set Char) -> ( Char, Char ) -> Map.Map Char (Set.Set Char)
dependencyMap map ( master, slave ) =
  map
    & updateWith Set.empty id master
    & updateWith Set.empty (Set.insert master) slave


completeOrdered :: String -> Set.Set Char -> Map.Map Char (Set.Set Char) -> String
completeOrdered sequence completed dependencies =
  case nextAvailable completed dependencies of
    Just next ->
      completeOrdered (next:sequence) (Set.insert next completed) dependencies
    Nothing -> sequence


nextAvailable :: Set.Set Char -> Map.Map Char (Set.Set Char) -> Maybe Char
nextAvailable completed dependencies =
  dependencies
    & Map.toList
    & filter (not . (\key -> Set.member key completed) . fst)
    & filter (diff completed . snd)
    & fmap fst
    & safeHead


diff :: Set.Set Char -> Set.Set Char -> Bool
diff completed dependents =
  completed
    & Set.difference dependents
    & Set.null
