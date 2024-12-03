module Main where

import System.Environment
import System.Directory


parseLines :: [String] -> [[Integer]]
parseLines [] = []
parseLines (x:xs) =  readAll x : parseLines xs


readAll :: String -> [Integer]
readAll "" = [] 
readAll  s = map read (words s)


isSafe :: [Integer] -> Bool
isSafe [] = False
isSafe ns = do
  let mx = maximum (changes ns)
  let inc = all (==True) (increasing ns)
  let dec = all (==True) (decreasing ns)
  mx <= 3 && (inc || dec)


changes :: [Integer] -> [Integer]
changes [] = []
changes [_] = []
changes (x:y:ys) = abs (x - y) : changes (y:ys)


increasing :: [Integer] -> [Bool]
increasing [] = []
increasing [_] = []
increasing (x:y:ys) = (x < y) : increasing (y:ys)


decreasing :: [Integer] -> [Bool]
decreasing [] = []
decreasing [_] = []
decreasing (x:y:ys) = (x > y) : decreasing (y:ys)


main :: IO ()
main = do
  args <- getArgs
  exists <- doesFileExist (head args)
  if exists then do
    rawInput <- readFile (head args) 
    let inputLines = lines rawInput
    let parsedLines = parseLines inputLines
    let safeRecords = filter isSafe parsedLines
    print (length safeRecords)
  else print "File does not exist!"
