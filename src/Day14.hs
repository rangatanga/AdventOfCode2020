module Day14 (partOne, partTwo)
where

import Data.List.Split ( splitOn )
import Data.List.Utils
import Data.List

partOne :: IO ()
partOne = do
    x <- lines <$> readFile "Inputs/14_input.txt"
    print (splitOn " = " (head x))
    let (mem,_) = foldl eval ([], "") x
            where eval (mem, msk) instr = let z = splitOn " = " instr
                                          in if head z == "mask" then (mem, concat (tail z))
                                             else if take 4 (head z) == "mem[" then (updateMemory mem (formatMemInstr z msk), msk)
                                                  else error "unexpected instruction"
    let memVal = foldl memsum 0 mem
                 where memsum tot (_,val) = tot+val
    print memVal   

partTwo :: IO ()
partTwo = do
    x <- lines <$> readFile "Inputs/14_input.txt"
    print (splitOn " = " (head x))
    let (mem,_) = foldl' eval ([], "") x
            where eval (mem, msk) instr = let z = splitOn " = " instr
                                          in if head z == "mask" then (mem, concat (tail z))
                                             else if take 4 (head z) == "mem[" then (foldl' updateMemory mem (formatMemInstr2 z msk), msk)
                                                  else error "unexpected instruction"
    let memVal = foldl' memsum 0 mem
                 where memsum tot (_,val) = tot+val
    print memVal   

updateMemory :: [(Int, Int)] -> (Int, Int)-> [(Int, Int)]
updateMemory [] (addr, value) =  [(addr,value)]
updateMemory ((m1, m2):ms) (addr, value) = if m1 == addr then (addr,value):ms
                                           else (m1, m2): updateMemory ms (addr, value)

formatMemInstr :: [String] -> String -> (Int, Int)
formatMemInstr (i:ix) msk = let val = read (head ix) :: Int
                                addr = read (replace "]" "" (replace "mem[" "" i)) :: Int
                            in (addr, updateValue val msk)

formatMemInstr2 :: [String] -> String -> [(Int, Int)]
formatMemInstr2 (i:ix) msk = let val = read (head ix) :: Int
                                 addr = read (replace "]" "" (replace "mem[" "" i)) :: Int
                            in [(a, val) | a <- updateValue2 addr msk]

updateValue :: Int -> String -> Int
updateValue val msk = let binVal = intToBin val
                          padBinVal = replicate (36 - length binVal) 0 ++ binVal
                       in binToInt (applyMask padBinVal msk)

updateValue2 :: Int -> String -> [Int]
updateValue2 val msk = let binVal = intToBin val
                           padBinVal = replicate (36 - length binVal) 0 ++ binVal
                       in map binToInt (applyMask2 padBinVal msk)


applyMask :: [Int] -> String -> [Int]
applyMask val msk = [if msk!!x == 'X' then val!!x else read [msk!!x] :: Int | x <- [0..35]]

applyMask2 :: [Int] -> String -> [[Int]]
applyMask2 val msk = let y = [if msk!!x == 'X' then 9 else if msk!!x == '1' then 1 else val!!x | x <- [0..(length msk)-1]]
                     in applyXMask2 y


applyXMask2 :: [Int]-> [[Int]]
applyXMask2 xs = if 9 `elem` xs then applyXMask2 (replaceFirst xs 0) ++ applyXMask2 (replaceFirst xs 1)
                 else [xs]


replaceFirst :: [Int] -> Int -> [Int]
replaceFirst [] _ = []
replaceFirst (x:xs) y = if x==9 then y:xs else x:replaceFirst xs y


intToBin :: Int -> [Int]
intToBin 0 = []
intToBin n  | n `mod` 2 == 1 = intToBin (n `div` 2) ++ [1]
            | otherwise = intToBin (n `div` 2) ++ [0]

binToInt :: [Int] -> Int
binToInt xs = let y = foldr eval (0,0) xs
                      where eval x (tot, pwr) = (tot + (x * (2^pwr)), pwr+1)
              in fst y
