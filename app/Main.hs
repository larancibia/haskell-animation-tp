module Main where

import Data.Char (isSpace)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Conversions as Text
import Lib(Env(..), State(..), animate)
import System.Environment
import System.IO
import Control.Monad

reverseWords :: String -> String
reverseWords a =  a

main :: IO ()
--main = animate (Env (10, 10)) (State (9, 10) (1, 1))

replaceStringAtIndex :: Int -> String -> String -> String
replaceStringAtIndex index replacement str
      | all isSpace replacement = str
      | otherwise = strHead ++ 
                    replacement ++ 
                    drop (length replacement) strAfter
  where (strHead, strAfter) = splitAt index str


replaceStringAtIndex3 :: Int -> String -> String -> String
replaceStringAtIndex3 index replacement str
      | all isSpace replacement = str
      | otherwise =
           strHead ++ 
                    zipWith (\x y -> if x==' ' then y; else x) replacement strAfter ++ 
                    drop (length replacement) strAfter
  where (strHead, strAfter) = splitAt index str
replaceStringAtIndex2 :: Int -> String -> String -> String
replaceStringAtIndex2 index replacement str
      | all isSpace replacement = str
      | otherwise = zipWith (\x y -> case y of 
          ' '->  y 
          x -> x) replacement str
  where (strHead, strAfter) = splitAt index str

returnScenarioLine :: [String] -> [String] -> [String]
returnScenarioLine anyLine scenarioLine = scenarioLine

putCactusAtPosition :: [Int] -> [String] -> [String] -> [String]
putCactusAtPosition positions cactusLines scenarioLines
  | not (null positions) = 
      let position = head positions 
          updatedList = drop 1 positions
      in putCactusAtPosition updatedList cactusLines (zipWith (replaceStringAtIndex3 position) cactusLines scenarioLines)
  | otherwise = scenarioLines

main = do
    scenarioFile <- readFile "app/scenario.txt"
    cactusFile <- readFile "app/cactus.txt"
    dinoFile <- readFile "app/dino.txt"
    let scenarioLines = lines scenarioFile
    let dinoLines = lines dinoFile
    let cactusLines = lines cactusFile

    let scenarioWithCactus = putCactusAtPosition [1,12,17] cactusLines scenarioLines
    let scenariowithdinasour = putCactusAtPosition [1] dinoLines scenarioWithCactus

    mapM_ putStrLn scenariowithdinasour