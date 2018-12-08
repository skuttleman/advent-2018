module Main where
import Data.Function
import Day1
import Day2
import Day3
import System.Environment

run :: ([String] -> String) -> Int -> IO String
run f day = day
  & show
  & (\day -> day ++ ".txt")
  & (++) "resources/day"
  & readFile
  & fmap (f . lines)

daySteps :: [( [String] -> String, [String] -> String )]
daySteps = [( Day1.step1, Day1.step2 ),
            ( Day2.step1, Day2.step2 ),
            ( Day3.step1, Day3.step2 )]

dayStep :: [String] -> ( Int, Int )
dayStep (day:step:_) =
  ( (read day) - 1, (read step) - 1 )

runner :: ( Int, Int ) -> IO String
runner ( day, step ) = 
  let
    ( f1, f2 ) = daySteps
      & drop day
      & head
  in
    run (if step == 0 then f1 else f2) (day + 1)

main :: IO ()
main = fmap dayStep getArgs >>= runner >>= putStrLn
