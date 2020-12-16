module Day15 (partOne)
where

import qualified Data.Map as M
import Data.List
import Data.Maybe

partOne :: IO ()
partOne = do
    let inp = [13,0,10,12,1,5,8]
    let (m,_) = foldl' add (M.empty,0) (init inp)
                where add (m,cnt) i = (M.insert i (cnt+1) m, cnt+1)
    let (inp', m') = play (last inp) m (length inp)                
    print ( inp')
    --print m'

play :: Int -> M.Map Int Int -> Int -> (Int, M.Map Int Int)
play inp m 30000000 = (inp, m)
play inp m trn = let lkup = M.lookup inp m
                 in if isNothing lkup 
                    then let m' = M.insert inp trn m 
                         in play 0 m' (trn+1)
                    else let pos = trn - fromJust lkup
                             m' = M.insert inp trn m
                         in play pos m' (trn+1)