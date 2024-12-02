module Main where

import System.Environment
import System.Directory
import Data.Text(Text, splitOn, unpack)
import Data.List(sort)

difference :: [(Integer, Integer)] -> [Integer]
difference [] = []
difference (n:ns) =  do
  let mx = max (fst n) (snd n);
  let mn = min (fst n) (snd n);
  (mx - mn) : difference ns


parseNums :: [String] -> ([Integer], [Integer])
parseNums [] = ([], [])
parseNums (line:xs) = 
  let ns = words line
  in case ns of
    [x, y] ->
      let (firstList, secondList) = parseNums xs
      in (read x : firstList, read y : secondList)
    _ -> parseNums xs


totalDistance :: ([Integer], [Integer]) -> Integer
totalDistance ([], []) = 0
totalDistance (firstList, secondList) = do
  let sortedNums = zip (sort firstList) (sort secondList)
  let differences = difference sortedNums
  sum differences


countItemOccurances ::  Integer -> [Integer] -> Int
countItemOccurances _ [] = 0
countItemOccurances x y = length (filter (\n -> (n == x)) y) * (fromIntegral x)


similarityScore :: [Integer] -> [Integer] -> Int
similarityScore [] _ = 0
similarityScore _ [] = 0
similarityScore (x:xs) y = countItemOccurances x y + similarityScore xs y


main :: IO ()
main = do
  x <- getArgs
  exists <- doesFileExist (head x)
  if exists then do
    raw_input <- readFile (head x)
    let input_lines = lines raw_input
    let parsed_nums = parseNums input_lines
    let result = totalDistance parsed_nums
    let simScore = similarityScore (fst parsed_nums) (snd parsed_nums)
    print ("Total Distances: " ++ (show result))
    print ("Similarity Score: " ++ (show simScore))
  else
    print "File not found."
