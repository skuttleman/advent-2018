module Day12 (step1, step2) where
import Data.Function
import Data.List
import qualified Data.Set as Set
import Utils.Regex

data Plants = Plants ( Int, Int ) (Set.Set Int) deriving (Eq, Ord)


instance Show Plants where
  show (Plants ( lower, upper ) set) =
    [lower..upper]
      & fmap (toChar . (flip Set.member set))


step1 :: [String] -> String
step1 (init:_:fns) =
  let
    state = parseInit init
    f = makeFns fns
  in
    state
      & recur 20 f
      & sumPlants
      & show


step2 :: [String] -> String
step2 _ = "tbd"


parseInit :: String -> Plants
parseInit s =
  let
    (init:_) = s #= "initial state: ([#\\.]+)"
  in
    init
      & fmap toBool
      & zip [0..]
      & foldl toPlants (Plants ( 0, 0 ) Set.empty)


toPlants :: Plants -> ( Int, Bool ) -> Plants
toPlants (Plants ( lower, upper ) set ) ( i, True ) = Plants ( min i lower, max i upper ) (Set.insert i set)
toPlants plants _ = plants


makeFns :: [String] -> Plants -> Plants
makeFns rules =
  rules
    & unify
    & applyFn


applyFn :: (( Bool, Bool, Bool, Bool, Bool ) -> Bool) -> Plants -> Plants
applyFn f (Plants ( lower, upper ) set) =
  [lower - 2..upper + 2]
    & foldl (nextPlants f set) (Plants ( 0, 0 ) Set.empty)


nextPlants :: (( Bool, Bool, Bool, Bool, Bool ) -> Bool) -> Set.Set Int -> Plants -> Int -> Plants
nextPlants f set plants i =
   ( Set.member (i - 2) set, Set.member (i - 1) set, Set.member i set, Set.member (i + 1) set, Set.member (i + 2) set )
    & f
    & (,) i
    & toPlants plants


unify :: [String] -> ( Bool, Bool, Bool, Bool, Bool ) -> Bool
unify patterns =
  patterns
    & filter (isSuffixOf "=> #")
    & fmap toRule
    & Set.fromList
    & flip Set.member


toRule :: String -> ( Bool, Bool, Bool, Bool, Bool )
toRule (a:b:c:d:e:_) =
  ( toBool a, toBool b, toBool c, toBool d, toBool e )


toBool :: Char -> Bool
toBool '#' = True
toBool _ = False


toChar :: Bool -> Char
toChar True = '#'
toChar False = '.'

recur :: Int -> (a -> a) -> a -> a
recur times f a
  | times <= 0 = a
  | otherwise = recur (times - 1) f (f a)


sumPlants :: Plants -> Int
sumPlants (Plants _ set) =
  set
    & Set.toList
    & foldl (+) 0
