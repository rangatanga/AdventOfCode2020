module Day13 (partOne, partTwo)
where

import Data.List.Split ( splitOn )

partOne :: IO ()
partOne = do
    x <- lines <$> readFile "Inputs/13_input.txt"
    let t = read (head x) :: Int
    let y = map (leastGreater t) (format (splitOn "," (last x))) 
    let (z1, z2) = foldl comp (t*10,0) y 
                   where comp (u1,u2) (v1,v2)= if u1<v1 then (u1,u2) else (v1,v2)
    print ((z1-t)*z2)

partTwo :: IO ()
partTwo = do
    x <- lines <$> readFile "Inputs/13_input.txt"
    let y = format2 (splitOn "," (last x)) 0
    let (z1,_) = head y
    let y' = foldl sort [] y
             where sort xs u = shuffle xs u
    let y'' = foldl minTime (1,1) y'
              where minTime (strt, stp) (p, offSet) = head [(x, (p*stp)) | x <- [strt, (strt+stp)..(p*stp)], (x+offSet) `mod` p == 0]
    print (y'')

format :: [String] -> [Int] 
format [] = []
format (x:xs) = if x == "x" then format xs else (read x :: Int) : format xs

format2 :: [String] -> Int -> [(Int,Int)]
format2 [] _ = []
format2 (x:xs) cnt = if x == "x" then format2 xs (cnt+1) else (read x :: Int, cnt) : format2 xs (cnt+1)

leastGreater :: Int -> Int -> (Int, Int)
leastGreater tgt x = let y = tgt `div` x
                     in if y*x >= tgt then (y*x, x) else ((y+1)*x, x)


shuffle :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
shuffle [] y = [y]
shuffle ((x1,x2):xs) (y1,y2) = if y1 >= x1 
                               then (y1,y2) : ((x1,x2):xs) 
                               else (x1,x2) : shuffle xs (y1,y2)