module Main where

import Data.Char (isSpace)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Conversions as Text
import Lib(Env(..), State(..), animate)
import System.Environment
import System.IO
import Control.Monad
import Control.Concurrent (threadDelay)
import System.Console.ANSI

main :: IO ()
--main = animate (Env (10, 10)) (State (9, 10) (1, 1))

replaceStringAtIndex :: Int -> String -> String -> String
replaceStringAtIndex index replacement str
      | all isSpace replacement = str
      | otherwise =
           strHead ++
                    zipWith (\x y -> if x==' ' then y; else x) replacement strAfter ++
                    drop (length replacement) strAfter
  where (strHead, strAfter) = splitAt index str

putImageAtPositions :: [Int] -> [String] -> [String] -> [String]
putImageAtPositions positions cactusLines scenarioLines
  | not (null positions) =
      let position = head positions
          updatedList = drop 1 positions
      in putImageAtPositions updatedList cactusLines (zipWith (replaceStringAtIndex position) cactusLines scenarioLines)
  | otherwise = scenarioLines

updatePositions:: [Int]->[Int]
updatePositions = map (\x -> if x-2<=0 then x+150 else x-1)


draw::[Int]->[String]->[String]->[String]->IO ()
draw posiciones cactusLines scenarioLines dinoLines = 
    let scenarioWithCactus = putImageAtPositions posiciones cactusLines scenarioLines
        scenariowithdinasour = putImageAtPositions [1] dinoLines scenarioWithCactus
    in mapM_ putStrLn scenariowithdinasour

animate2:: [Int]->[String]->[String]->[String]->IO ()
animate2 positions cactusLines scenarioLines dinoLines = 
   draw positions cactusLines scenarioLines dinoLines  >> threadDelay 10000 >> clearScreen >> animate2 (updatePositions positions) cactusLines scenarioLines dinoLines


main = do
    scenarioFile <- readFile "app/scenario.txt"
    cactusFile <- readFile "app/cactus.txt"
    dinoFile <- readFile "app/dino.txt"
    let scenarioLines = lines scenarioFile
    let dinoLines = lines dinoFile
    let cactusLines = lines cactusFile

    animate2 [28,42,50,18,67,89,123,129,139,159,164,123] cactusLines scenarioLines dinoLines