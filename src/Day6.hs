module Day6 (partOne, partTwo, intersects) 
where

import System.IO 
--import Data.Char 
import Data.List.Split
import Data.List
import Text.Regex.Posix

partOne :: IO ()
partOne = do
    inh <- openFile "Inputs/06_input.txt" ReadMode
    x <- processFile inh "" []
    print (sum (map length (map processList1 x)))
    hClose inh

partTwo :: IO ()
partTwo = do
    inh <- openFile "Inputs/06_input.txt" ReadMode
    x <- processFile inh "" []
    print (sum (map length (map processList2 x)))
    hClose inh

processFile :: Handle -> String -> [String] -> IO [String]
processFile h x y = do
    eof <- hIsEOF h
    if eof then return (concat (y:[[x]]))
    else do
        l <- hGetLine h
        let l' = "\""++l++"\""
        let x' = if null l then "" else x ++ "|" ++ read l' :: String
        let y' = if null l then (y:[[x]]) else [y]
        processFile h x' (concat y')

processList1 :: String -> [Char]
processList1 xs = [x | x <- ['a'..'z'], x `elem` xs]

processList2 :: String -> [Char]
processList2 xs = [x | x <- intersects (splitOn "|" xs) ['a'..'z']]


intersects :: [String] -> String -> String
intersects [] y = y
intersects ("":xs) y = intersects xs y
intersects (x:xs) y = intersects xs (x `intersect` y)
