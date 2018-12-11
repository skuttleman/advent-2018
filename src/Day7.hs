module Day7 (step1, step2) where
import Data.Char
import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils.Lists
import Utils.Maps
import Utils.Regex
import Utils.Tuples


step1 :: [String] -> String
step1 lines =
  lines
    & fmap dependency
    & foldl dependencyMap Map.empty
    & completeOrdered Set.empty ""
    & reverse


step2 :: [String] -> String
step2 lines =
  lines
    & fmap dependency
    & foldl dependencyMap Map.empty
    & timeParallel 5 60 Set.empty 0 Map.empty
    & show


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


completeOrdered :: Set.Set Char -> String -> Map.Map Char (Set.Set Char) -> String
completeOrdered completed sequence dependencies =
  case nextAvailable completed dependencies of
    [] ->
      sequence
    (next:_) ->
      completeOrdered (Set.insert next completed) (next:sequence) dependencies


nextAvailable :: Set.Set Char -> Map.Map Char (Set.Set Char) -> [Char]
nextAvailable completed dependencies =
  dependencies
    & Map.toList
    & filter (not . (\key -> Set.member key completed) . fst)
    & filter (diff completed . snd)
    & fmap fst


withNextAvailable :: Set.Set Char -> Map.Map Char (Set.Set Char) -> Map.Map Int ( Int, Maybe Char ) -> Int -> [Int] -> Map.Map Int ( Int, Maybe Char )
withNextAvailable completed dependencies inProgress timeOffset ids =
  let
    inProgressSteps =
      inProgress
        & Map.toList
        & keep (snd . snd)
        & Set.fromList
  in
    nextAvailable completed dependencies
      & filter (not . (\char -> Set.member char inProgressSteps))
      & zip ids
      & fmap (map2of2 (\char -> ( toTimeNeeded timeOffset char, Just char )))
      & foldl (\m ( k, v ) -> Map.insert k v m) inProgress


diff :: Set.Set Char -> Set.Set Char -> Bool
diff completed dependents =
  completed
    & Set.difference dependents
    & Set.null


timeParallel :: Int -> Int -> Set.Set Char -> Int -> Map.Map Int ( Int, Maybe Char ) -> Map.Map Char (Set.Set Char) -> Int
timeParallel workers timeOffset completed timer inProgress dependencies
  | Map.null inProgress =
    timeParallel workers timeOffset completed timer (Map.fromList (fmap (\id -> ( id, ( 0, Nothing ) )) [1..workers])) dependencies
  | otherwise =
    let
      ( nextCompleted, nextInProgress ) =
        inProgress
          & Map.toList
          & filter ((== 0) . fst . snd)
          & fmap fst
          & withNextAvailable completed dependencies inProgress timeOffset
          & updateTick completed
    in
      if Set.size nextCompleted == Map.size dependencies then
        timer + 1
      else
        timeParallel workers timeOffset nextCompleted (timer + 1) nextInProgress dependencies


updateTick :: Set.Set Char -> Map.Map Int ( Int, Maybe Char ) -> ( Set.Set Char, Map.Map Int ( Int, Maybe Char ) )
updateTick completed inProgress =
  inProgress
    & Map.toList
    & foldl collectFinished ( completed, Map.empty )


collectFinished :: ( Set.Set Char, Map.Map Int ( Int, Maybe Char ) ) -> ( Int, ( Int, Maybe Char ) ) -> ( Set.Set Char, Map.Map Int ( Int, Maybe Char ) )
collectFinished ( completed, inProgress ) ( id, ( count, maybeStep ) ) =
  case maybeStep of
    Nothing ->
      ( completed, Map.insert id ( 0, Nothing ) inProgress )
    Just step ->
      if count <= 1 then
        ( Set.insert step completed, Map.insert id ( 0, Nothing ) inProgress )
      else
        ( completed, Map.insert id ( count - 1, maybeStep ) inProgress )


toTimeNeeded :: Int -> Char -> Int
toTimeNeeded offset letter =
  offset + ord letter - (ord 'A' - 1)
