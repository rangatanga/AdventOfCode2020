module Day12 (partOne, partTwo)
where

partOne :: IO ()
partOne = do
    x <- readFile "Inputs/12_input.txt"
    let x' = map format (lines x)
    let (d,n,e) = loop x' ('E',0,0)
    print ([d]++","++ (show n)++","++show e)
    print (abs n + abs e)

partTwo :: IO ()
partTwo = do
    x <- readFile "Inputs/12_input.txt"
    let x' = map format (lines x)
    let ((shpN,shpE),(wpN, wpE)) = loop2 x' ((0,0),(1,10))
    print (show shpN++","++show shpE++","++show wpN++","++show wpE)
    print (abs shpN + abs shpE)

format :: String -> (Char,Int)
format s = let mv = read (drop 1 s) :: Int
           in (head s, mv)

loop :: [(Char,Int)] -> (Char,Int, Int) -> (Char,Int, Int) 
loop [] curPos = curPos
loop (x:xs) curPos = loop xs (update curPos x)

loop2 :: [(Char,Int)] -> ((Int, Int), (Int,Int)) -> ((Int, Int), (Int,Int))
loop2 [] curPos = curPos
loop2 (x:xs) curPos = loop2 xs (update2 curPos x)

update :: (Char,Int, Int) -> (Char,Int) -> (Char,Int,Int)
update (d,n,e) (d',v')  | d'=='N' || (d'=='F' && d=='N') =  (d,n+v',e)
                        | d'=='S' || (d'=='F' && d=='S') =  (d,n-v',e)                            
                        | d'=='E' || (d'=='F' && d=='E') =  (d,n,e+v')
                        | d'=='W' || (d'=='F' && d=='W') =  (d,n,e-v')
                        | d'=='L' = (chgDir d (v' `div` (-90)), n, e)
                        | d'=='R' = (chgDir d (v' `div` 90), n, e)

update2 :: ((Int, Int), (Int,Int)) -> (Char,Int) -> ((Int, Int), (Int,Int)) 
update2 ((shipN,shipE),(wpN, wpE)) (d',v')  | d'=='N' = ((shipN, shipE), (wpN+v', wpE))
                                            | d'=='S' = ((shipN, shipE), (wpN-v', wpE))
                                            | d'=='E' = ((shipN, shipE), (wpN, wpE+v'))
                                            | d'=='W' = ((shipN, shipE), (wpN, wpE-v'))
                                            | d'=='L' = ((shipN, shipE), moveWayPt (wpN, wpE) (v' `div` (-90) `mod` 4))
                                            | d'=='R' = ((shipN, shipE), moveWayPt (wpN, wpE) ((v' `div` 90) `mod` 4))
                                            | d'=='F' = ((shipN + (v'*wpN), shipE + (v'*wpE)), (wpN, wpE))


chgDir :: Char -> Int -> Char
chgDir d x = intToDir (((dirToInt d) + x) `mod` 4)

dirToInt :: Char -> Int
dirToInt d = case d of
                'N' -> 0
                'E' -> 1
                'S' -> 2
                'W' -> 3

intToDir :: Int -> Char
intToDir i = case i of
                0 -> 'N'
                1 -> 'E'
                2 -> 'S'
                3 -> 'W'
                (-1) -> 'W'
                (-2) -> 'S'
                (-3) -> 'E'

moveWayPt :: (Int,Int) -> Int -> (Int,Int)
moveWayPt (wpN, wpE) rot = case rot of
                            0 -> (wpN, wpE)
                            1 -> (-wpE, wpN)
                            2 -> (-wpN, -wpE)
                            3 -> (wpE, -wpN)
