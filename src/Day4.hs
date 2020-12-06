module Day4 (partOne, partTwo) 
where

import System.IO 
--import Data.Char 
import Data.List.Split
import Text.Regex.Posix

partOne :: IO ()
partOne = do
    inh <- openFile "Inputs/04_input.txt" ReadMode
    x <- processFile inh "" []
    print (length (processList1 x))
    hClose inh

partTwo :: IO ()
partTwo = do
    inh <- openFile "Inputs/04_input.txt" ReadMode
    x <- processFile inh "" []
    print (length (processList2 x))
    hClose inh

processFile :: Handle -> String -> [String] -> IO [String]
processFile h x y = do
    eof <- hIsEOF h
    if eof then return (concat (y:[[drop 1 x]]))
    else do
        l <- hGetLine h
        let l' = "\""++l++"\""
        let x' = if null l then "" else x ++ " " ++ read l' :: String
        let y' = if null l then (y:[[drop 1 x]]) else [y]
        processFile h x' (concat y')

processList1 :: [String] -> [String]
processList1 xs = [x | x <- xs, validDetails1 x]

processList2 :: [String] -> [String]
processList2 xs = [x | x <- xs, validDetails2 x]

validDetails1 :: String -> Bool
validDetails1 x = let 
                    flds = map (splitOn ":") (splitOn " " x)
                    cnt = validFieldCount1 flds 0
                 in if (cnt == 8 || (cnt == 7 && not (hasField flds "cid"))) then True else False

validDetails2 :: String -> Bool
validDetails2 x = let 
                    flds = map (splitOn ":") (splitOn " " x)
                    cnt = validFieldCount2 flds 0
                 in if (cnt == 8 || (cnt == 7 && not (hasField flds "cid"))) then True else False

validFieldCount1 :: [[String]] -> Int -> Int
validFieldCount1 [] cnt = cnt
validFieldCount1 ((y:_):xs) cnt = let cnt' = cnt + case y of
                                                    "byr" -> 1
                                                    "iyr" -> 1
                                                    "eyr" -> 1
                                                    "hgt" -> 1
                                                    "hcl" -> 1
                                                    "ecl" -> 1
                                                    "pid" -> 1
                                                    "cid" -> 1
                                                    otherwise -> 0
                                  in validFieldCount1 xs cnt'

validFieldCount2 :: [[String]] -> Int -> Int
validFieldCount2 [] cnt = cnt
validFieldCount2 ((y:ys):xs) cnt = let cnt' = cnt + case y of
                                                    "byr" -> let z = read (head ys) :: Int
                                                             in if z >= 1920 && z <= 2002 then 1 else 0
                                                    "iyr" -> let z = read (head ys) :: Int
                                                             in if z >= 2010 && z <= 2020 then 1 else 0
                                                    "eyr" -> let z = read (head ys) :: Int
                                                             in if z >= 2020 && z <= 2030 then 1 else 0
                                                    "hgt" -> let z = head ys :: String
                                                             in if regExp (y++"cm") z then
                                                                    let z' = read (reverse (drop 2 (reverse z))) :: Int
                                                                    in if z' >= 150 && z' <= 193 then 1 else 0
                                                                else if regExp (y++"in") z then
                                                                        let z' = read (reverse (drop 2 (reverse z))) :: Int
                                                                        in if z' >= 59 && z' <= 76 then 1 else 0
                                                                     else 0
                                                    "hcl" -> let z = head ys :: String
                                                             in if regExp y z then 1 else 0
                                                    "ecl" -> let z = head ys :: String
                                                             in if z=="amb" ||z=="blu" ||z=="brn" ||z=="gry" ||z=="grn" ||z=="hzl" ||z=="oth" then 1 else 0
                                                    "pid" -> let z = (head ys :: String)
                                                             in if regExp y z then 1 else 0
                                                    "cid" -> 1
                                                    otherwise -> 0
                                  in validFieldCount2 xs cnt'

hasField :: [[String]] -> String -> Bool
hasField [] _ = False
hasField ((y:_):xs) s = y == s || hasField xs s
{-
validValue :: String -> String -> Int
validValue x y = case x of 
    -}

regExp :: String -> String -> Bool
regExp x y = case x of
                "pid" -> (y =~ "[0-9]" :: Int) == 9
                "hcl" -> (y =~ "#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]" :: Bool) && length y == 7
                "hgtcm" -> (y =~ "[0-9]cm" :: Bool)
                "hgtin" -> (y =~ "[0-9]in" :: Bool)                