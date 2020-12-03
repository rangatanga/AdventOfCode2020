module Day3 (partOne, partTwo) 
where

import System.IO ( hClose, hIsEOF, openFile, hGetLine, Handle, IOMode(ReadMode) )
import Data.Char ()


partOne :: IO ()
partOne = do
    inh <- openFile "Inputs/03_input.txt" ReadMode
    processInput1 inh 0 0
    hClose inh

partTwo :: IO ()
partTwo = do
    inh <- openFile "Inputs/03_input.txt" ReadMode
    processInput2 inh 0 (0,0,0,0,0)
    hClose inh

processInput1 :: Handle -> Int -> Int -> IO ()
processInput1 h rowId treeCnt  = do
    eof <- hIsEOF h
    if eof then print treeCnt
    else do
        x <- hGetLine h
        let x' = concat(replicate 35 x)
        let treeCnt' = treeCnt + if x'!!(rowId*3) == '#' then 1 else 0
        processInput1 h (rowId+1) treeCnt'


processInput2 :: Handle -> Int -> (Int,Int,Int,Int,Int) -> IO ()
processInput2 h rowId (treeCnt1,treeCnt2,treeCnt3,treeCnt4,treeCnt5)  = do
    eof <- hIsEOF h
    if eof then print (treeCnt1,treeCnt2,treeCnt3,treeCnt4,treeCnt5,treeCnt1*treeCnt2*treeCnt3*treeCnt4*treeCnt5)
    else do
        x <- hGetLine h
        let x' = concat(replicate 135 x)
        let treeCnt1' = treeCnt1 + if x'!!rowId == '#' then 1 else 0
        let treeCnt2' = treeCnt2 + if x'!!(rowId*3) == '#' then 1 else 0
        let treeCnt3' = treeCnt3 + if x'!!(rowId*5) == '#' then 1 else 0
        let treeCnt4' = treeCnt4 + if x'!!(rowId*7) == '#' then 1 else 0
        let treeCnt5' = treeCnt5 + if even rowId && x'!!(rowId `div` 2) == '#' then 1 else 0
        processInput2 h (rowId+1) (treeCnt1',treeCnt2',treeCnt3',treeCnt4',treeCnt5')

