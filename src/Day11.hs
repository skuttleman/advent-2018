module Day11 (step1, step2) where
import Data.Function
import qualified Data.Map as Map
import Data.Vector2D
import Utils.Lists


type Vector = Vector2D Int


step1 :: [String] -> String
step1 lines =
  lines
    & head
    & read
    & build 300 300
    & fst
    & fmap (vector2DToTuple . fst)
    & coord


coord :: Maybe ( Int, Int ) -> String
coord Nothing = ""
coord (Just ( x, y )) =
  show x ++ "," ++ show y


step2 :: [String] -> String
step2 _ = "tbd"


build :: Int -> Int -> Int -> ( Maybe ( Vector, Int ), Map.Map Vector Int )
build width height serial =
  [ vector2D x y | y <- [1..height], x <- [1..width] ]
    & foldl (build' serial) ( Nothing, Map.empty )


build' :: Int -> ( Maybe ( Vector, Int ), Map.Map Vector Int ) -> Vector -> ( Maybe ( Vector, Int ), Map.Map Vector Int )
build' serial ( highest, map ) vector =
  let
    ( x, y ) = vector2DToTuple vector
    power = toPower serial x y
    map' = Map.insert vector power map
  in
    case ( x >= 3 && y >= 3, highest ) of
      ( True, Just high ) -> ( Just (toHighest (vector - 2) map' high), map' )
      ( True, Nothing ) -> ( Just (toTotal (vector - 2) map'), map' )
      _ -> ( highest, map' )


toHighest :: Vector -> Map.Map Vector Int -> ( Vector, Int ) -> ( Vector,  Int )
toHighest vector map highest@( position, power ) =
  let
    other@( position2, power2 ) = toTotal vector map
  in
    if power2 > power then
      other
    else
      highest


toTotal :: Vector -> Map.Map Vector Int -> ( Vector, Int )
toTotal vector map =
  let
    ( x, y ) = vector2DToTuple vector
  in
    [ vector2D x' y' | y' <- [y..y + 2], x' <- [x..x + 2] ]
      & foldl (totalPower map) 0
      & (,) vector


totalPower :: Map.Map Vector Int -> Int -> Vector -> Int
totalPower map total vector =
  case Map.lookup vector map of
    Just value -> value + total
    Nothing -> total


toPower :: Int -> Int -> Int -> Int
toPower serial x y =
  let
    rackID = x + 10
    power = rackID * y
    power' = (power + serial) * rackID
    hundreds = digit power' 2
  in
    hundreds - 5


digit :: Int -> Int -> Int
digit power index =
  power
    & show
    & reverse
    & drop index
    & headOr '0'
    & pure
    & read
