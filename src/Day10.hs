module Day10 (partOne, partTwo)
where

import Data.List.Ordered
import Data.List
import Data.List.Split ( splitOn )
import Data.List.Utils

partOne :: IO ()
partOne = do
    x <- readFile "Inputs/10_input_test.txt"
    let x' = toInt (lines x)
    let y = sort (x'++[0]++[(maximum x')+3])
    print ( (ordList y))
    print (product (map length (group (sort (ordList y)))))


partTwo :: IO ()
partTwo = do
    x <- readFile "Inputs/10_input.txt"
    let x' = toInt (lines x)
    let y = sort (x'++[0]++[(maximum x')+3])
    let y' = removeItems [0,1] (map length (splitOn [3] (ordList y)))
    print y'
    print (product (replace [3] [4] (replace [4] [7] y')))

toInt :: [String] -> [Int]
toInt xs = [read x :: Int | x <- xs]

ordList :: Num a => Ord a => [a] -> [a]
ordList xs = [x2 - x1 | x1 <- xs, x2 <-xs, x1 < x2, x2 - x1 < 4, null (between xs x1 x2)]

between :: Ord a => [a] -> a -> a -> [a]
between xs min max = [x| x <- xs, x > min, x < max]

removeItems :: Eq a => [a] -> [a] -> [a]
removeItems _ [] = []
removeItems xs (y:ys) | y `elem` xs = removeItems xs ys
                      | otherwise = y : removeItems xs ys


