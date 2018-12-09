module Utils.Maps (frequencies, updateWith) where
import qualified Data.Map as Map


frequencies :: (Ord a, Eq a) => [a] -> Map.Map a Int
frequencies = foldr (updateWith 0 (+ 1)) Map.empty


updateWith :: (Ord k) => v -> (v -> v) -> k -> Map.Map k v -> Map.Map k v
updateWith starter f k m =
  if (Map.member k m) then
    Map.adjust f k m
  else
    Map.insert k (f starter) m
