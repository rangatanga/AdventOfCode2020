module Day2 (partOne, partTwo) 
where

import Data.Char ()
import qualified Data.ByteString.Char8 as L
import Data.Bool () 

partOne :: IO ()
partOne = do
    inh <- L.readFile "Inputs/02_input.txt"
    processPwd1 0 (processInput inh)

partTwo :: IO ()
partTwo = do
    inh <- L.readFile "Inputs/02_input.txt"
    processPwd2 0 (processInput inh)

processInput :: L.ByteString -> [[L.ByteString]]
processInput = map (L.split ' ') . L.lines

processPwd1 :: Int -> [[L.ByteString]] -> IO ()
processPwd1 cntr [] = print cntr
processPwd1 cntr ((rule:chr:pwd):xs) = do
    let cntr' = cntr + checkPwd1 (L.split '-' rule) chr (head pwd)
    processPwd1 cntr' xs

processPwd2 :: Int -> [[L.ByteString]] -> IO ()
processPwd2 cntr [] = print cntr
processPwd2 cntr ((rule:chr:pwd):xs) = do
    let cntr' = cntr + checkPwd2 (L.split '-' rule) chr (head pwd)
    processPwd2 cntr' xs

checkPwd1 :: [L.ByteString] -> L.ByteString -> L.ByteString -> Int
checkPwd1 (minOcc:maxOcc) chr pwd = do
    let pwd' = L.unpack pwd
    let min = read (L.unpack minOcc) :: Int
    let max = read (L.unpack (head maxOcc)) :: Int
    let chr' = head (L.unpack chr) :: Char
    let chrCnt = chrCount chr' pwd' 0
    if chrCnt >= min && chrCnt <= max then 1 else 0

checkPwd2 :: [L.ByteString] -> L.ByteString -> L.ByteString -> Int
checkPwd2 (minOcc:maxOcc) chr pwd = do
    let pwd' = L.unpack pwd
    let min = read (L.unpack minOcc) :: Int
    let max = read (L.unpack (head maxOcc)) :: Int
    let chr' = head (L.unpack chr) :: Char
    if (pwd'!!(min-1) == chr') /= (pwd'!!(max-1) == chr') then 1 else 0

chrCount :: Char -> [Char] -> Int -> Int
chrCount _ [] cnt = cnt
chrCount chr (x:xs) cnt = if x==chr then  chrCount chr xs cnt+1 else chrCount chr xs cnt

