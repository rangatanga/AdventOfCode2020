module Day9 (partOne, partTwo, loop, hasContigSum)
where

partOne :: IO ()
partOne = do
    x <- readFile "Inputs/09_input.txt"
    let x' = toInt (lines x)
    print (processOne x')

partTwo :: IO ()
partTwo = do
    x <- readFile "Inputs/09_input.txt"
    let x' = toInt (lines x)
    let y = loop x' 0
    print (show (minimum y) ++ ", " ++ show (maximum y))
    print (show (minimum y + maximum y))
    

toInt :: [String] -> [Int]
toInt xs = [read x :: Int | x <- xs]

processOne :: [Int] -> Int
processOne xs = let y = hasSum (take 25 xs) (xs!!25)
                in if null y then xs!!25 else processOne (drop 1 xs) -- ++y

hasSum :: [Int] -> Int -> [Int]
hasSum xs tgt = [x+y | x <- xs, y <- xs, x /= y, x+y==tgt]

hasContigSum :: [Int] -> Int -> [Int] -> [Int]
hasContigSum xs tgt ys | sum ys > tgt = []
                       | sum ys == tgt = ys
                       | otherwise = let ys' = ys++[head xs] 
                                     in hasContigSum (drop 1 xs) tgt ys'

loop :: [Int] -> Int -> [Int]
loop xs cnt = let y = hasContigSum xs 57195069 []
              in if null y then loop (drop 1 xs) (cnt+1) else y