module Day11 (partOne, partTwo)
where

import Data.List.Utils

partOne :: IO ()
partOne = do
    x <- readFile "Inputs/11_input.txt"
    let x' = lines x
    loop x' updateSeats

partTwo :: IO ()
partTwo = do
    x <- readFile "Inputs/11_input.txt"
    let x' = lines x
    loop x' updateSeats2

updateSeats :: [String] -> [String]
updateSeats xs = [[case xs!!j!!i of
                    '.' -> '.' 
                    'L' -> if occupiedNeighbours xs (i,j) == 0 then '#' else 'L'
                    '#' -> if occupiedNeighbours xs (i,j) >= 4 then 'L' else '#' | i <- [0..(length (head xs)-1)]] | j <- [0..(length xs-1)]]

occupiedNeighbours :: [String] -> (Int,Int) -> Int
occupiedNeighbours xs (i,j) = length ['#' | u <- [-1,0,1], v <- [-1,0,1], not (u==0 && v==0), u+i>=0, u+i<length (head xs), j+v>=0, j+v<length xs, xs!!(j+v)!!(i+u)=='#']

updateSeats2 :: [String] -> [String]
updateSeats2 xs = [[case xs!!j!!i of
                    '.' -> '.' 
                    'L' -> if occupiedNeighbours2 xs (i,j) == 0 then '#' else 'L'
                    '#' -> if occupiedNeighbours2 xs (i,j) >= 5 then 'L' else '#' | i <- [0..(length (head xs)-1)]] | j <- [0..(length xs-1)]]

occupiedNeighbours2 :: [String] -> (Int,Int) -> Int
occupiedNeighbours2 xs (i,j) = sum [getNearestNeighbour xs (i,j) (u,v) | u <- [-1,0,1], v <- [-1,0,1], not (u==0 && v==0)]


loop :: [String] -> ([String] -> [String]) -> IO ()
loop xs f = do
    --putStrLn (unlines xs)
    let xs' = f xs
    if xs==xs' then print (sum (map (count "#") xs)) else loop xs' f

count :: String -> String -> Int
count c xs = length xs - length (replace c "" xs)

getNearestNeighbour :: [String] -> (Int,Int) -> (Int,Int) -> Int
getNearestNeighbour xs (i,j) (i',j') | i+i'<0 || j+j'<0 || i+i' >= length (head xs) || j+j' >= length xs = 0
                      | xs!!(j+j')!!(i+i') == 'L' = 0
                      | xs!!(j+j')!!(i+i') == '#' = 1
                      | otherwise = getNearestNeighbour xs (i+i', j+j') (i',j')