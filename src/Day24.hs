module Day24
where

import qualified Data.Map as M
--import Data.IntMap ((!))
import Data.List


partOne :: IO ()
partOne = do
    dirs <-  length . filter odd . map length . group . sort . map (process (0,0)) . map format . lines <$> readFile "Inputs/24_input.txt"
    print dirs
    where
        process (x,y) [] = (x,y)
        process (x,y) (d:ds) = let t | d=="e" = (x+1,y)
                                     | d=="w" = (x-1,y)
                                     | d=="ne" = (x+0.5,y+0.5)
                                     | d=="sw" = (x-0.5,y-0.5)
                                     | d=="se" = (x+0.5,y-0.5)
                                     | d=="nw" = (x-0.5,y+0.5)
                                 in process t ds
        format :: String -> [String]
        format [] = []
        format (x:xs) = if  x `elem` "ew" then (""++[x]) : format xs
                        else ([x]++[head xs]) : (format $ drop 1 xs)



partTwo :: IO ()
partTwo = do
    day0 <- map (process (0,0)) . map format . lines <$> readFile "Inputs/24_input.txt"
    let m = M.empty
    let m0 = foldl upd m day0
             where upd mp t = case M.lookup t mp of
                                Just b -> M.insert t (not b) mp
                                _ -> M.insert t True mp
    print (loop' m0 0)
    where
        --loop' :: M.Map (Double, Double) Bool -> Int -> M.Map (Double, Double) Bool
        loop' m cnt = if cnt==100 then length . filter (==True) . map snd $ M.toList m
                      else let r = getRange (0,0,0,0) (map fst $ M.toList m)
                               (_,m') = foldl' eval (m,m) r
                                        where eval (m1, m2) (x,y) = let isBlk = case M.lookup (x,y) m1 of
                                                                                    Just b -> b
                                                                                    _      -> False
                                                                        bn = blackNeighbours (x,y) m1
                                                                    in if isBlk && (bn==0 || bn > 2) then (m1, M.insert (x,y) False m2)
                                                                        else if (not isBlk) && bn==2 then (m1, M.insert (x,y) True m2)
                                                                             else (m1, m2)
                           in loop' m' (cnt+1)

        loop :: M.Map (Double, Double) Bool -> M.Map (Double, Double) Bool -> [(Double, Double)] -> M.Map (Double, Double) Bool
        loop _ m' [] = m'
        loop m m' ((x,y):xs) = let isBlk = case M.lookup (x,y) m of
                                            Just b -> b
                                            _      -> False
                                   bn = blackNeighbours (x,y) m
                               in if isBlk && (bn==0 || bn > 2) then loop m (M.insert (x,y) False m) xs
                                  else if (not isBlk) && bn==2 then loop m (M.insert (x,y) True m) xs
                                       else loop m m' xs
        
        getRange :: (Double,Double,Double,Double) -> [(Double,Double)] -> [(Double,Double)]
        getRange (mnX,mxX,mnY,mxY) [] = [(x,y) | x <- [mnX-1.5,mnX-1..mxX+1.5], y <- [mnY-1.5,mnY-1..mxY+1.5], abs (x - fromIntegral (round x))==abs (y - fromIntegral (round y))]
        getRange (mnX,mxX,mnY,mxY) ((x,y):ts) = getRange (minimum ([x]++[mnX]),maximum ([x]++[mxX]),minimum ([y]++[mnY]), maximum ([y]++[mxY])) ts
        
        blackNeighbours (x,y) m = foldl' eval 0 [(1,0), (-1,0), (0.5,0.5), (0.5,-0.5), (-0.5,0.5), (-0.5,-0.5)]
                                  where eval cnt (x',y') = case M.lookup (x+x', y+y') m of
                                                              Just True -> cnt+1
                                                              _ -> cnt
        process (x,y) [] = (x,y)
        process (x,y) (d:ds) = let t | d=="e" = (x+1,y)
                                     | d=="w" = (x-1,y)
                                     | d=="ne" = (x+0.5,y+0.5)
                                     | d=="sw" = (x-0.5,y-0.5)
                                     | d=="se" = (x+0.5,y-0.5)
                                     | d=="nw" = (x-0.5,y+0.5)
                                 in process t ds
        format :: String -> [String]
        format [] = []
        format (x:xs) = if  x `elem` "ew" then (""++[x]) : format xs
                        else ([x]++[head xs]) : (format $ drop 1 xs)
