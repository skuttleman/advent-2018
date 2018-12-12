module Day8 (step1, step2) where
import Data.Function
import qualified Data.NTree as NTree


step1 :: [String] -> String
step1 lines =
  lines
    & head
    & words
    & fmap read
    & buildTree
    & snd
    & foldl (foldl (+)) 0
    & show


step2 :: [String] -> String
step2 lines =
  lines
    & head
    & words
    & fmap read
    & buildTree
    & snd
    & NTree.foldUp totalRoot 0
    & show


buildTree :: [Int] -> ( [Int], NTree.NTree [Int] )
buildTree (childCount:metaCount:more) =
  let
    ( ints, children ) = buildChildren childCount ( more, [] )
  in
    ( (drop metaCount ints), NTree.parent children (take metaCount ints) )


buildChildren :: Int -> ( [Int], [NTree.NTree [Int]] ) -> ( [Int], [NTree.NTree [Int]] )
buildChildren 0 result = result
buildChildren count ( ints, children ) =
  let
    ( next, child ) = buildTree ints
  in
    buildChildren (count - 1) ( next, child:children )


totalRoot :: [Int] -> [NTree.NTree [Int]] -> Int -> Int
totalRoot metaData [] result = foldl (+) result metaData
totalRoot metaData children result =
  let
    len = length children
  in
  metaData
    & fmap (\i -> nthOr (len - i) NTree.empty children)
    & foldl (NTree.foldUp totalRoot) result


nthOr :: Int -> a -> [a] -> a
nthOr 0 _ (head:_) = head
nthOr n value children
  | n > 0 = nthOr 0 value (drop n children)
nthOr _ value _ = value
