module Day10 (step1, step2) where
import Data.Function
import Data.List
import Data.Mover
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Vector2D
import Utils.Regex

type Vector = Vector2D Int
type TimeVector = Mover Vector
data Grid = Grid [TimeVector] deriving (Eq)

instance Show Grid where
  show (Grid (tv:tvs)) =
    let
      pos = position tv
      ( min, max, set ) =
        tvs
          & foldl findMinMax ( pos, pos, Set.empty )
      ( minX, minY ) = vector2DToTuple min
      ( maxX, maxY ) = vector2DToTuple max
    in
      [ vector2D x y | y <- [maxY, maxY - 1..minY], x <- [maxX, maxX - 1..minX]]
        & foldl (draw set) ( maxY, [""] )
        & snd
        & intercalate "\n"


step1 :: [String] -> String
step1 = show . Grid . (\( tvs, amt ) -> fmap (move amt) tvs) . step'


step2 :: [String] -> String
step2 = show . fst . vector2DToTuple . snd . step'


step' :: [String] -> ( [TimeVector], Vector )
step' lines =
  let
    tvs = fmap toTimeVector lines
    amt = floor (2 ** 20)
  in
    tvs
      & foldl (closestToOrigin tvs) Map.empty
      & Map.toList
      & sortBy (\( _, a ) ( _, b ) -> compare a b)
      & head
      & fst
      & (,) tvs


findClosest :: TimeVector -> Vector -> Vector -> Vector
findClosest tv amt closest =
  if dist (position (move amt tv)) < dist (position (move closest tv)) then
    amt
  else
    closest


dist :: Vector -> Double
dist vec =
  let
    ( x, y ) = vector2DToTuple vec
  in
    sqrt $ fromIntegral (x * x + y * y)


emptyVector :: Vector
emptyVector = vector2D 0 0


toTimeVector :: String -> TimeVector
toTimeVector s =
  let
    (x1:y1:x2:y2:_) =
      s #= "position=< *(-?[0-9]+), *(-?[0-9]+)> velocity=< *(-?[0-9]+), *(-?[0-9]+)>"
      & fmap read
  in
    mover (vector2D x1 y1) (vector2D x2 y2)


findMinMax :: ( Vector, Vector, Set.Set Vector ) -> TimeVector -> ( Vector, Vector, Set.Set Vector )
findMinMax ( min, max, set ) tv =
  let
    pos = position tv
  in
    ( minVector2D min pos, maxVector2D max pos, Set.insert pos set )


closestToOrigin :: [TimeVector] -> Map.Map Vector Vector -> TimeVector -> Map.Map Vector Vector
closestToOrigin tvs map tv =
  let
    pos = position tv
    v = velocity tv
    ( x1, y1 ) = vector2DToTuple pos
    ( x2, y2 ) = vector2DToTuple v
    closestX = if x2 == 0 then x1 else (div x1 x2) * (-1)
    closestY = if y2 == 0 then y1 else (div y1 y2) * (-1)
    start = min closestX closestY
    closest = foldl (findClosest tv) (vector2D start start) [vector2D x x | x <- [start + 1..max closestX closestY]] 
  in
    case Map.lookup closest map of
      Nothing -> Map.insert closest (findSize (fmap (move closest) tvs)) map
      _ -> map


findSize :: [TimeVector] -> Vector
findSize tvs =
  let
    ( min, max, _ ) = foldl findMinMax ( emptyVector, emptyVector, Set.empty ) tvs
  in
    max - min


draw :: Set.Set Vector -> ( Int, [String] ) -> Vector -> ( Int, [String] )
draw set ( currentY, result@(line:more) ) vector =
  let
    ( x, y ) = vector2DToTuple vector
  in
    case ( currentY == y, Set.member vector set ) of
      ( True, True ) -> ( y, ('#':line):more )
      ( True, False ) -> ( y, (' ':line):more )
      ( False, True ) -> ( y, "#":result )
      ( False, False ) -> ( y, " ":result )
