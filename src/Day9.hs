module Day9 (step1, step2) where
import Data.Function
import Data.List
import qualified Data.Map as Map
import Utils.Lists
import Utils.Fns
import Utils.Maps
import Utils.Regex


data Marbles = Marbles [Int] Int Int Int deriving (Eq, Show)
data Players = Players Int (Map.Map Int Int) Int deriving (Eq, Show)
data Game = Game Marbles Players deriving (Eq)

instance Show Game where
  show (Game (Marbles board index _ _) (Players _ scores _)) =
    (show' board index "") ++ " :: " ++ (show scores)
    -- show scores


show' :: [Int] -> Int -> String -> String
show' [] index shown = shown
show' (marble:marbles) 0 shown =
  show' marbles (-1) (shown ++ " (" ++ show marble ++ ")")
show' (marble:marbles) index shown =
  show' marbles (index - 1) (shown ++ " " ++ show marble)


step1 :: [String] -> String
step1 lines =
  lines
    & head
    & parseInput
    & initGame
    & play
    & highScore
    & show


step2 :: [String] -> String
step2 _ = "tbs"


parseInput :: String -> [Int]
parseInput s =
  s #= "([0-9]+) players; last marble is worth ([0-9]+) points"
    & fmap read


initGame :: [Int] -> Game
initGame (players:marbles:_) =
  Game
    & call (Marbles [0] 0 0 marbles)
    & call (Players 0 Map.empty players)


play :: Game -> Game
play game@(Game marbles players)
  | isOver game = game
  | otherwise =
    Game
      & call (nextMarbles marbles)
      & call (nextPlayers marbles players)
      & play


highScore :: Game -> Int
highScore (Game _ (Players _ scores _)) =
  scores
    & Map.toList
    & fmap snd
    & sortBy (flip compare)
    & head


nextMarbles :: Marbles -> Marbles
nextMarbles (Marbles board index lastPlayed total) =
  let
    marble = lastPlayed + 1
    index' = (nextIndex board marble index)
  in
    Marbles
      & call (nextBoard marble index' board)
      & call index'
      & call marble
      & call total


nextPlayers :: Marbles -> Players -> Players
nextPlayers marbles (Players lastPlayer scores total) =
  let
    player = nextPlayer lastPlayer total
  in
    Players
      & call player
      & call (nextScore marbles player scores)
      & call total


nextPlayer :: Int -> Int -> Int
nextPlayer lastPlayer total
  | lastPlayer == total = 1
  | otherwise = lastPlayer + 1


nextScore :: Marbles -> Int -> Map.Map Int Int -> Map.Map Int Int
nextScore (Marbles board index lastPlayed total) player scores =
  let
    played = lastPlayed + 1
    dropped = nthOr (nextIndex board played index) 0 board
  in
    if scorable played then
      updateWith 0 (+ (dropped + played)) player scores
    else
      scores


nextBoard :: Int -> Int -> [Int] -> [Int]
nextBoard marble index board
  | scorable marble =
    (take index board) ++ (drop (index + 1) board)
  | otherwise =
    (take index board) ++ (marble:drop index board)


nextIndex :: [Int] -> Int -> Int -> Int
nextIndex board marble index
  | scorable marble = cycleIndex (index - 7) (length board)
  | otherwise = cycleIndex (index + 2) (length board)


isOver :: Game -> Bool
isOver (Game (Marbles _ _ lastPlayed total) _) =
  lastPlayed == total


scorable :: Int -> Bool
scorable marble = mod marble 23 == 0


cycleIndex :: Int -> Int -> Int
cycleIndex nextIndex len =
  if nextIndex < 0 then
    nextIndex + len
  else if nextIndex > len then
    nextIndex - len
  else
    nextIndex
