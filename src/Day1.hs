module Day1 (partOne, partTwo) 
where

import System.IO ( hClose, hIsEOF, openFile, hGetLine, Handle, IOMode(ReadMode) )
import Data.Char ()

partOne :: IO ()
partOne = do
    inh <- openFile "Inputs/01_input.txt" ReadMode
    x <- processFile inh []
    print (processList1 x)
    hClose inh

partTwo :: IO ()
partTwo = do
    inh <- openFile "Inputs/01_input.txt" ReadMode
    x <- processFile inh []
    print (processList2 x)
    hClose inh

processFile :: Handle -> [Int] -> IO [Int]
processFile h i = do
    eof <- hIsEOF h
    if eof then return i
    else do
        x <- hGetLine h
        let i' = i ++ [read x :: Int]
        processFile h i'

processList1 :: [Int] -> [(Int,Int,Int)]
processList1 xs = [(x,y,x*y) | x <- xs, y <- xs, x+y == 2020]

processList2 :: [Int] -> [(Int,Int,Int,Int)]
processList2 xs = [(x,y,z,x*y*z) | x <- xs, y <- xs, z <- xs, x+y+z == 2020]
