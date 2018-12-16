module Day9 (step1, step2) where
import Data.Function
import Data.List
import qualified Data.MarbleBoard as MB
import qualified Data.Map as Map
import Utils.Lists
import Utils.Maps
import Utils.Regex


data Marbles = Marbles MB.MarbleBoard Int Int deriving (Eq, Show)
data Players = Players Int (Map.Map Int Int) Int deriving (Eq, Show)
data Game = Game Marbles Players deriving (Eq, Show)


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
    (Marbles (MB.fromList [0]) 0 marbles)
    (Players 0 Map.empty players)


play :: Game -> Game
play game@(Game marbles players)
  | isOver game = game
  | otherwise =
    Game
      (nextMarbles marbles)
      (nextPlayers marbles players)
      & play


playToWin :: Game -> Int
playToWin = highScore . play


updateMarbles :: (Int -> Int) -> Game -> Game
updateMarbles f (Game (Marbles board lastPlayed total) players) =
  Game (Marbles board lastPlayed $ f total) players


highScore :: Game -> Int
highScore (Game _ (Players _ scores _)) =
  scores
    & Map.toList
    & fmap snd
    & sortBy (flip compare)
    & head


nextMarbles :: Marbles -> Marbles
nextMarbles (Marbles board lastPlayed total) =
  let
    marble = lastPlayed + 1
  in
    Marbles
      (nextBoard marble board)
      marble
      total


nextPlayers :: Marbles -> Players -> Players
nextPlayers marbles (Players lastPlayer scores total) =
  let
    player = nextPlayer lastPlayer total
  in
    Players
      player
      (nextScore marbles player scores)
      total


nextPlayer :: Int -> Int -> Int
nextPlayer lastPlayer total
  | lastPlayer == total = 1
  | otherwise = lastPlayer + 1


nextScore :: Marbles -> Int -> Map.Map Int Int -> Map.Map Int Int
nextScore (Marbles board lastPlayed _) player scores =
  let
    played = lastPlayed + 1
    removed = MB.getAt (played - 5) board
    score = played + removed
  in
    if scorable played then
      updateWith 0 (+ score) player scores
    else
      scores


nextBoard :: Int -> MB.MarbleBoard -> MB.MarbleBoard
nextBoard marble board
  | scorable marble =
    MB.removeAt (marble - 5) board
  | scorable (marble - 1) =
    let
      after = MB.next (marble - 5) board
    in
      MB.insertAfter after marble board
  | otherwise =
    let
      after = MB.next (marble - 1) board
    in
      MB.insertAfter after marble board


isOver :: Game -> Bool
isOver (Game (Marbles _ lastPlayed total) _) =
  lastPlayed == total


scorable :: Int -> Bool
scorable marble = mod marble 23 == 0
