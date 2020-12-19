module Day17
where

import qualified Data.Map as M
import Data.Maybe

type Pt1 = (Int, Int, Int)
type Pt2 = (Int, Int, Int, Int)

partOne :: IO ()
partOne = do
    xs <- lines <$> readFile "Inputs/17_input.txt"
    let m = M.empty
    let (m',_) = foldl process (m, (0,0,0)) xs
                 where process (mp,(x,y,z)) l = (formatInput l (x,y,z) mp, (0,y+1,0))
    print (length (filter (snd) (M.toList (loop m' ((0,0,0), (length (head xs),length xs,1)) 1))))

partTwo :: IO ()
partTwo = do
    xs <- lines <$> readFile "Inputs/17_input.txt"
    let m = M.empty
    let (m',_) = foldl process (m, (0,0,0,0)) xs
                 where process (mp,(x,y,z,w)) l = (formatInput2 l (x,y,z,w) mp, (0,y+1,0,0))
    print (length (filter (snd) (M.toList (loop2 m' ((0,0,0,0), (length (head xs),length xs,1,1)) 1))))

formatInput :: [Char] -> Pt1 -> M.Map Pt1 Bool -> M.Map Pt1 Bool
formatInput [] _ m = m
formatInput (c:cs) (x,y,z) m = let m' = if c=='#' then M.insert (x,y,z) True m else m
                               in formatInput cs (x+1,y,z) m'

formatInput2 :: [Char] -> Pt2 -> M.Map Pt2 Bool -> M.Map Pt2 Bool
formatInput2 [] _ m = m
formatInput2 (c:cs) (x,y,z,w) m = let m' = if c=='#' then M.insert (x,y,z,w) True m else m
                                 in formatInput2 cs (x+1,y,z,w) m'

loop :: M.Map Pt1 Bool -> (Pt1,Pt1) -> Int -> M.Map Pt1 Bool
loop m _ 7 = m
loop m ((mnX,mnY,mnZ),(mxX,mxY,mxZ)) cnt = loop (processCube m ((mnX-1,mnY-1,mnZ-1), (mxX+1,mxY+1,mxZ+1))) ((mnX-1,mnY-1,mnZ-1), (mxX+1,mxY+1,mxZ+1)) (cnt+1)

loop2 :: M.Map Pt2 Bool -> (Pt2,Pt2) -> Int -> M.Map Pt2 Bool
loop2 m _ 7 = m
loop2 m ((mnX,mnY,mnZ,mnW),(mxX,mxY,mxZ,mxW)) cnt = loop2 (processCube2 m ((mnX-1,mnY-1,mnZ-1,mnW-1), (mxX+1,mxY+1,mxZ+1,mxW+1))) ((mnX-1,mnY-1,mnZ-1,mnW-1), (mxX+1,mxY+1,mxZ+1,mxW+1)) (cnt+1)


processCube :: M.Map Pt1 Bool -> (Pt1,Pt1) -> M.Map Pt1 Bool
processCube m ((mnX,mnY,mnZ),(mxX,mxY,mxZ))= foldl process m [(x,y,z) | x <- [mnX..mxX], y <- [mnY..mxY], z <- [mnZ..mxZ]]
                                             where process m' pos = case M.lookup pos m of
                                                                        Just True -> if (activeNeighbours m pos) `elem` [2,3] then m' else M.insert pos False m'
                                                                        _ -> M.insert pos ((activeNeighbours m pos) `elem` [3]) m'

processCube2 :: M.Map Pt2 Bool -> (Pt2,Pt2) -> M.Map Pt2 Bool
processCube2 m ((mnX,mnY,mnZ,mnW),(mxX,mxY,mxZ,mxW))= foldl process m [(x,y,z,w) | x <- [mnX..mxX], y <- [mnY..mxY], z <- [mnZ..mxZ], w <- [mnW..mxW]]
                                                      where process m' pos = case M.lookup pos m of
                                                                                Just True -> if (activeNeighbours2 m pos) `elem` [2,3] then m' else M.insert pos False m'
                                                                                _ -> M.insert pos ((activeNeighbours2 m pos) `elem` [3]) m'


activeNeighbours :: M.Map Pt1 Bool -> Pt1 -> Int
activeNeighbours m (x,y,z) = length [(x+x',y+y',z+z') | x' <- [-1..1], y' <- [-1..1], z' <- [-1..1], not (x'==0 && y'==0 && z'==0), M.lookup (x+x',y+y',z+z') m == Just True]

activeNeighbours2 :: M.Map Pt2 Bool -> Pt2 -> Int
activeNeighbours2 m (x,y,z,w) = length [(x+x',y+y',z+z') | x' <- [-1..1], y' <- [-1..1], z' <- [-1..1], w' <- [-1..1], not (x'==0 && y'==0 && z'==0 && w'==0), M.lookup (x+x',y+y',z+z',w+w') m == Just True]
