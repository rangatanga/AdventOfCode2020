module Day22
where

import Data.List.Split (linesBy)
import Data.Numbers.Primes ( wheelSieve )

partOne :: IO ()
partOne = do
    cards <- map (drop 1) . linesBy (=="") . lines <$> readFile "Inputs/22_input.txt"
    let res = (loop1 (map read $ head cards) (map read $ last cards) 0)
    print (sum [res!!(length res-x) * x | x <- [length res, length res-1..1]])
    print ("")

loop1 :: [Int] -> [Int] -> Int -> [Int]
loop1 p1 [] _ = p1
loop1 [] p2 _ = p2
loop1 p1 p2 cnt = let (x,y) = (head p1, head p2)
             in if x > y then loop1 ((drop 1 p1) ++ [x,y]) (drop 1 p2) (cnt+1) else loop1 (drop 1 p1) ((drop 1 p2) ++ [y,x]) (cnt+1)



partTwo :: IO ()
partTwo = do
    cards <- map (drop 1) . linesBy (=="") . lines <$> readFile "Inputs/22_input.txt"
    let (win,res,h) = loop2 (map read $ head cards) (map read $ last cards) []
    print (sum [res!!(length res-x) * x | x <- [length res, length res-1..1]])
    --print (win, res, h)
    print ("")

loop2 :: [Int] -> [Int] -> [(Int,Int)] -> (Int, [Int], [(Int, Int)])
loop2 p1 [] h = (1, p1, h)
loop2 [] p2 h = (2, p2, h)
loop2 p1 p2 h | existsInHistory p1 p2 h = (1, p1, h)
              | (length p1-1 >= head p1) && (length p2-1 >= head p2) = let (win,_,_) = loop2 (take (head p1) (drop 1 p1)) (take (head p2) (drop 1 p2)) []
                                                                       in if win==1 then loop2 ((drop 1 p1) ++ [head p1, head p2]) (drop 1 p2) (h++[(encode p1, encode p2)])
                                                                          else loop2 (drop 1 p1)  (drop 1 p2 ++ [head p2, head p1]) (h++[(encode p1, encode p2)])
              | otherwise = let (x,y) = (head p1, head p2)
                            in if x > y then loop2 ((drop 1 p1) ++ [x,y]) (drop 1 p2) (h++[(encode p1, encode p2)]) 
                               else loop2 (drop 1 p1) ((drop 1 p2) ++ [y,x]) (h++[(encode p1, encode p2)])
    where
        existsInHistory p1' p2' history' = (encode p1', encode p2') `elem` history'
        encode u = sum (zipWith (*) u (wheelSieve 6))