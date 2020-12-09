module Day7 (partOne, partTwo, bagSplit, getBag, processTwo) 
where

import Data.List.Split ( splitOn )
import Data.List.Utils ( replace )
import Data.List ( isInfixOf )
import qualified Data.Set as S
--import qualified Data.Text as T
--import qualified  Data.Text as T
--import Basement.String

partOne :: IO ()
partOne = do
    x <- readFile "Inputs/07_input.txt"
    let x' = replace "." "" (replace " bag" "" (replace " bags" "" x))
    print (length (S.fromList (processOne (map (splitOn " contain ") (lines x')) ["shiny gold"] [])))

partTwo :: IO ()
partTwo = do
    x <- readFile "Inputs/07_input.txt"
    let x' = replace ", " "," (replace "." "" (replace " bag" "" (replace " bags" "" x)))
    let y = concat (getBag "shiny gold" (map ( splitOn " contain ") (lines x')))
    let x'' = map (splitOn " contain ") (lines x')
    print (processTwo x'' 0 (1, "shiny gold")-1)


processOne :: [[String]] -> [String] -> [String]-> [String]
processOne _ [] y = y
processOne xs (s:ss) y = let s' = searchList xs s []
                         in processOne xs (ss++s') y++s'

processTwo :: [[String]] -> Int -> (Int, String) -> Int
processTwo _ _ (0, _) = 0
processTwo xs m (y1, y2) = y1 + y1 * sum (map (processTwo xs m) (concat (getBag y2 xs)))

searchList :: [[String]] -> String -> [String]-> [String]
searchList [] _ y = y
searchList (x:xs) s y = let y' = if s `isInfixOf` last x then y++[head x] else y
                        in searchList xs s y'

bagSplit :: String -> (Int, String)
bagSplit s = let i = if s=="no other" then 0 else read (head (splitOn " " s)) :: Int
             in (i, drop (length (show i)+1) s)


getBag :: String -> [[String]] -> [[(Int, String)]]
getBag s xs = [map bagSplit (splitOn "," (last x)) | x <- xs, head x == s]