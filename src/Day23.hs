module Day23
where

--import Data.List
import qualified Data.IntMap.Strict as IM
import Data.IntMap ((!))

nIters = 10000000
n = 1000000
dec 0 = n-1
dec x = x-1

partOne :: IO ()
partOne = print f 

f = r1 * r2
    where
        xs = [8,5,3,1,9,2,6,4,7] --[3,8,9,1,2,5,4,6,7]
        xs' = map (+ (-1)) xs ++ [length xs .. n-1]
        xs'' = IM.fromList(zip xs' (tail $ cycle xs'))
        (r1:r2:_) = map (+1) $ mapToList 0 0 $ snd $ applyN nIters g (head xs', xs'')
        mapToList i0 i m = let i' = m ! i
                           in if i' == i0 then []
                              else i' : mapToList i0 i' m

g (current,m) = g' $ dec' $ dec current
    where
        x1     = m ! current
        x2     = m ! x1
        x3     = m ! x2
        next   = m ! x3
        dec' x = if x==x1 || x==x2 || x==x3 then dec' $ dec x else x
        g' x   = (next, IM.insert x3 (m ! x) $ IM.insert x x1 $ IM.insert current next m)  


applyN 0 f = id
applyN n f = foldr1 (.) $ replicate n f

{-
partOne :: IO ()
partOne = do
    let inp = [3,8,9,1,2,5,4,6,7]++[10..1000000] --test
    --let inp = [8,5,3,1,9,2,6,4,7]
    let m = M.fromList (zip inp (tail $ cycle inp))
    let (m',c) = applyN 10000000 f (m,head inp)
    
    --displayList m' 1 0
    let x = m' ! 1
    print (x, m' ! x)

f (m,current) = (M.insert x3 (m ! insPos) $ M.insert insPos x1 $ M.insert current cur' m, cur')
    where
        x1     = m ! current
        x2     = m ! x1
        x3     = m ! x2
        cur'   = m ! x3
        insPos = desc (current-1)
        desc x | x==0 = desc n
               | x==x1 || x==x2 || x==x3 = desc x-1
               | otherwise = x

displayList :: M.IntMap Int -> Int -> Int-> IO ()
displayList m x cnt = if cnt==9 then putStrLn []
                        else do
                        putStr $ show (x)
                        displayList m (m ! x) (cnt+1)





partTwo :: IO ()
partTwo = do
    --let inp = [3,8,9,1,2,5,4,6,7]++[10..1000000] --test
    --let inp = [8,5,3,1,9,2,6,4,7]
    let inp = [8,5,3,1,9,2,6,4,7]++[10..1000000]
    print (play inp 0)
    where
        play :: [Int] -> Int -> ([Int], [Int], [Int])
        play xs cnt = if cnt==4 then (take 30 xs, take 40 (dropWhile (<999999) xs), drop 999970 xs)
                      else let (s,e,l) = liftCups xs
                               d = dest (head xs) e
                           in play ((insertAt e l d)++[head xs]) (cnt+1)
        
        liftCups xs = (head xs, drop 4 xs, take 3 (drop 1 xs))
        dest x ys | x==1 = dest 1000001 ys
                  | (x-1) `elem` ys = x-1
                  | otherwise = dest (x-1) ys
        insertAt xs ys d = let start = takeWhile (/=d) xs
                           in start++[d]++ys++drop (length start+1) xs


applyN 0 f = id
applyN n f = foldr1 (.) $ replicate n f
-}