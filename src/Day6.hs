module Day6 (step1, step2) where
import Data.Function
import Data.List
import Data.Vector2D
import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils.Maps
import Utils.Regex
import Utils.Tuples


type Vector = Vector2D Int
data Cell = Empty | Closest Vector | MultiClose Vector deriving (Show)


step1 :: [String] -> String
step1 lines =
  lines
    & fmap toVector
    & foldr withSize ( Set.empty, Nothing, Nothing )
    & withGrid
    & fmap (\( _, (Closest position) ) -> position)
    & frequencies
    & Map.toList
    & fmap snd
    & sortBy (flip compare)
    & head
    & show


step2 :: [String] -> String
step2 lines =
  lines
    & fmap toVector
    & foldr withSize ( Set.empty, Nothing, Nothing )
    & withDistances
    & length
    & show


toVector :: String -> Vector
toVector s =
  let
    (x:y:_) = s #= "(-?[0-9]+), (-?[0-9]+)"
  in
    vector2D (read x) (read y)


withSize :: Vector -> ( Set.Set Vector, Maybe Vector, Maybe Vector ) -> ( Set.Set Vector, Maybe Vector, Maybe Vector )
withSize position ( positions, Nothing, Nothing ) =
  ( Set.insert position positions, Just position, Just (vector2D 0 0))
withSize position ( positions, Just topLeft, Just size ) =
  let
    nextTopLeft = minVector2D topLeft position
    nextSize = (maxVector2D (size + topLeft) position) - nextTopLeft
  in
    ( Set.insert position positions, Just nextTopLeft, Just nextSize )
withSize _ result = result


withGrid :: ( Set.Set Vector, Maybe Vector, Maybe Vector ) -> [( Vector, Cell )]
withGrid ( positions, Just topLeft, Just size) =
  let
    ( width, height ) = vector2DToTuple size
    pos =
      positions
        & Set.toList
        & fmap (+ (vector2D (-1) (-1)))
  in
    [( vector2D x y, position ) | y <- [0..height], x <- [0..width], position <- pos]
      & foldl markClosest Map.empty
      & removeInfinite size
      & removeMissing


withDistances :: ( Set.Set Vector, Maybe Vector, Maybe Vector ) -> [Vector]
withDistances ( positions, Just topLeft, Just size ) =
  let
    ( width, height ) = vector2DToTuple size
    ( x', y' ) = vector2DToTuple topLeft
  in
    [vector2D x y | y <- [y' - 1..y' + height + 1], x <- [x' - 1..x' + width + 1]]
      & filter (minDistance (Set.toList positions) (< 10000))


minDistance :: [Vector] -> (Int -> Bool) -> Vector -> Bool
minDistance positions pred position =
  positions
    & fmap (distance position)
    & foldl (+) 0
    & pred


markClosest :: Map.Map Vector Cell -> ( Vector, Vector ) -> Map.Map Vector Cell
markClosest grid ( location, position ) =
  case Map.lookup location grid of
    Nothing -> Map.insert location (Closest position) grid
    Just cell ->
      case cell of
        Closest current -> Map.insert location (pickClosest location current position) grid
        Empty -> Map.insert location (Closest position) grid
        MultiClose current -> Map.insert location (pickClosest' location current position) grid


pickClosest :: Vector -> Vector -> Vector -> Cell
pickClosest location position1 position2 =
  case compare (distance location position1) (distance location position2) of
    EQ -> MultiClose position1
    LT -> Closest position1
    GT -> Closest position2


pickClosest' :: Vector -> Vector -> Vector -> Cell
pickClosest' location position1 position2 =
  case compare (distance location position1) (distance location position2) of
    EQ -> MultiClose position1
    LT -> MultiClose position1
    GT -> Closest position2


distance :: Vector -> Vector -> Int
distance position1 position2 =
  let
    ( x1, y1 ) = vector2DToTuple position1
    ( x2, y2 ) = vector2DToTuple position2
  in
    abs (x1 - x2) + abs (y1 - y2)


removeInfinite :: Vector -> Map.Map Vector Cell -> Map.Map Vector Cell
removeInfinite size grid =
  grid
    & Map.toList
    & foldl (removeEdge size) grid


removeMissing :: Map.Map Vector Cell -> [( Vector, Cell )]
removeMissing grid =
  grid
    & Map.toList
    & filter (removeMissingClosest grid)


removeEdge :: Vector -> Map.Map Vector Cell -> ( Vector, Cell ) -> Map.Map Vector Cell
removeEdge size grid ( position, cell ) =
  case ( isEdge size position, cell ) of
    ( True, Closest closest ) ->
      Map.delete closest grid
    ( _, Empty ) ->
      Map.delete position grid
    ( _, MultiClose _ ) ->
      Map.delete position grid
    _ ->
      grid


removeMissingClosest :: Map.Map Vector Cell -> ( Vector, Cell ) -> Bool
removeMissingClosest grid ( position1, (Closest position2) ) =
  Map.member position2 grid


isEdge :: Vector -> Vector -> Bool
isEdge size position =
  let
    ( x1, y1 ) = vector2DToTuple size
    ( x2, y2 ) = vector2DToTuple position
  in
    x2 == 0 || y2 == 0 || x1 == x2 || y1 == y2


pprint :: ( Vector, Cell ) -> ( Vector, Char )
pprint ( position, cell ) =
  ( position,
    case cell of
      Closest pos ->
        case vector2DToTuple pos of
          ( 0, 0 ) -> 'a'
          ( 0, 5 ) -> 'b'
          ( 7, 2 ) -> 'c'
          ( 2, 3 ) -> 'd'
          ( 4, 4 ) -> 'e'
          ( 7, 8 ) -> 'f'
          _ -> '?'
      _ -> '.')
