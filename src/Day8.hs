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
    & fmap (foldr (+) 0)
    & foldr (+) 0
    & show


step2 :: [String] -> String
step2 _ = "tbd"


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

