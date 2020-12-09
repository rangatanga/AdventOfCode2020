module Day8 (partOne, partTwo, runOne, runTwo, loop)
where 

import Data.List.Split ( splitOn )
import Data.List.Utils ( replace )

partOne :: IO ()
partOne = do
    x <- readFile "Inputs/08_input.txt"
    let x' = (map (splitOn " ") (lines (replace "+" "" x)))
    runOne x' 0 0 []


partTwo :: IO ()
partTwo = do
    x <- readFile "Inputs/08_input.txt"
    let x' = (map (splitOn " ") (lines (replace "+" "" x)))
    loop x' "nop" 1
    loop x' "jmp" 1

runOne :: [[String]] -> Int -> Int -> [Int] -> IO ()
runOne ops acc curLine exLines | curLine `elem` exLines = print (show acc++" - halted")
                               | curLine == length ops = print (show acc++" - finished")
                               | otherwise = do
                                   let cmd = ops !! curLine
                                   let mv = read (last cmd)  :: Int
                                   let nextLine = case head cmd of
                                                    "acc" -> curLine+1
                                                    "jmp" -> curLine+mv
                                                    "nop" -> curLine+1
                                   let acc' = if head cmd == "acc" then acc+mv else acc
                                   let exLines' = exLines++[curLine]
                                   print ((show cmd) ++ (show acc))
                                   runOne ops acc' nextLine exLines'


runTwo :: [[String]] -> Int -> Int -> [Int] -> String -> Int-> Int -> (Int, String)
runTwo ops acc curLine exLines opChg nopChg nopCnt | curLine `elem` exLines = (acc,"halted")
                                             | curLine == length ops = (acc,"finished")
                                             | otherwise = do
                                                let cmd = ops !! curLine
                                                let mv = read (last cmd)  :: Int
                                                let nopCnt' = if head cmd==opChg then nopCnt+1 else nopCnt
                                                let nextLine = case head cmd of
                                                                    "acc" -> curLine+1
                                                                    "jmp" -> if nopCnt==nopChg && opChg=="jmp" then curLine+1 else curLine+mv
                                                                    "nop" -> if nopCnt==nopChg && opChg=="nop" then curLine+mv else curLine+1
                                                let acc' = if head cmd == "acc" then acc+mv else acc
                                                let exLines' = exLines++[curLine]
                                                --print ((show cmd) ++ (show acc))
                                                runTwo ops acc' nextLine exLines' opChg nopChg nopCnt'
loop :: [[String]] -> String -> Int -> IO ()
loop ops opChg nopChg = do 
    let (x1,x2) = runTwo ops 0 0 [] opChg nopChg 0
    if x2=="finished" || nopChg > length ops then print ("acc="++(show x1) ++ ", nopChg="++(show nopChg)++" - "++x2) else loop ops opChg (nopChg+1)
