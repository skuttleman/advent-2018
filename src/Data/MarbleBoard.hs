module Data.MarbleBoard (MarbleBoard(), fromList, getAt, insertAfter, next, removeAt, size, toList) where
import Data.Function
import Data.List
import qualified Data.Map as Map
import Utils.Lists
import Utils.Tuples


data MarbleBoard = MarbleBoard Int (Map.Map Int Int) deriving (Eq, Show)


fromList :: [Int] -> MarbleBoard
fromList [] = MarbleBoard 0 Map.empty
fromList items@(item:_) = MarbleBoard (length items) (fromList' item items Map.empty)


getAt :: Int -> MarbleBoard -> Int
getAt marble (MarbleBoard _ items) =
  case Map.lookup marble items of
    Just result -> result
    Nothing -> 0


insertAfter :: Int -> Int -> MarbleBoard -> MarbleBoard
insertAfter after item (MarbleBoard len items) =
  items
    & whenAt after item
    & Map.insert after item
    & MarbleBoard (len + 1)


next :: Int -> MarbleBoard -> Int
next index (MarbleBoard _ items) =
  case Map.lookup index items of
    Just next -> next
    Nothing -> 0


removeAt :: Int -> MarbleBoard -> MarbleBoard
removeAt marble board@(MarbleBoard len items) =
  let
    linked = next (next marble board) board
  in
    MarbleBoard (len - 1) (Map.insert marble linked items)


size :: MarbleBoard -> Int
size (MarbleBoard len _) = len


toList :: MarbleBoard -> [Int]
toList (MarbleBoard len items) = toList' len items Nothing


whenAt :: Int -> Int -> Map.Map Int Int -> Map.Map Int Int
whenAt after item m =
  case Map.lookup after m of
    Just marble -> Map.insert item marble m
    Nothing -> m


fromList' :: Int -> [Int] -> Map.Map Int Int -> Map.Map Int Int
fromList' _ [] m = m
fromList' hd (item:[]) m = Map.insert item hd m
fromList' hd (item1:items) m =
  let
    item2 = head items
  in
  m
    & Map.insert item1 item2
    & fromList' hd items


toList' 0 _ _ = []
toList' len items Nothing =
  items
    & Map.toList
    & head
    & fst
    & Just
    & toList' len items
toList' len items (Just next) =
  next:toList' (len - 1) items (Map.lookup next items)
