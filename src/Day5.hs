module Day5 (partOne, partTwo) 
where

import System.IO ( hClose, hIsEOF, openFile, hGetLine, Handle, IOMode(ReadMode) )
import Data.Char ()

partOne :: IO ()
partOne = do
    inh <- openFile "Inputs/05_input.txt" ReadMode
    x <- processFile inh []
    print (maximum (processList1 x))
    hClose inh

partTwo :: IO ()
partTwo = do
    inh <- openFile "Inputs/05_input.txt" ReadMode
    x <- processFile inh []
    print (processList2 x)
    hClose inh

processFile :: Handle -> [String] -> IO [String]
processFile h i = do
    eof <- hIsEOF h
    if eof then return i
    else do
        x <- hGetLine h
        processFile h (i ++ [x])

processList1 :: [String] -> [Int]
processList1 xs = [findSeat (take 7 x) (0,127) * 8 + findSeat (drop 7 x) (0,7) | x <- xs]

processList2 :: [String] -> [(Int,Int)]
processList2 xs = [(y1,y2) | y1 <- ys, y2 <- ys, y2-y1==2,  (y1+1) `notElem` ys]
                  where ys = [findSeat x (0,1023) | x <- xs]

findSeat :: [Char] -> (Int,Int) -> Int
findSeat [] (y1,_) = y1
findSeat (x:xs) (y1,y2) = let (y1',y2') = if x=='F' || x =='L' then (y1, (y1+y2-1) `div` 2) else ((y1+y2+1) `div` 2, y2)
                          in findSeat xs (y1',y2')

