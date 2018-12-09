module Utils.Maps (updateWith) where
import qualified Data.Map as Map


updateWith :: (Ord k) => v -> (v -> v) -> k -> Map.Map k v -> Map.Map k v
updateWith starter f k m =
  if (Map.member k m) then
    Map.adjust f k m
  else
    Map.insert k (f starter) m
