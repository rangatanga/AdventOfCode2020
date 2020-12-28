module Day19 
where

import Data.List.Split (linesBy, splitOn)
import qualified Data.Map as M
import qualified Parser as P
import Control.Applicative
import qualified Data.Set as S

partOne :: IO ()
partOne = do
    x <- linesBy (=="") . lines <$> readFile "Inputs/19_input.txt"
    let m = foldl imp M.empty (head x)
            where imp m x = let y = splitOn ": " x
                               in M.insert (read (head y)) (format (last y)) m
    --let mxL = maximum (map length (last x))
    --generate all possible words, limited to length <= max length (mxL) of words to be validated
    let (_,combos) = loop m (S.singleton "{0}", S.empty) (last x)
    print (length (filter (`elem` combos) (last x)))
    -- print (length combos)

partTwo :: IO ()
partTwo = do
    x <- linesBy (=="") . lines <$> readFile "Inputs/19_input_2.txt"
    --build map of format, e.g. 0 => ["{1}{2}", "{3}{4}"] or 5 => ["a"]
    let m = foldl imp M.empty (head x)
            where imp m x = let y = splitOn ": " x
                               in M.insert (read (head y)) (format (last y)) m
--    let mxL = maximum (map length (last x))
    let (_,combos) = loop m (S.singleton "{0}", S.empty) (last x)
    print (length (filter (`elem` combos) (last x)))
    -- print (length combos)

loop :: M.Map Int [String] -> (S.Set String, S.Set String) -> [String] -> (S.Set String, S.Set String)
loop m (xs, ys) msgs = let (xs',ys') = S.partition ('{' `elem`) (S.fromList (concatMap (substitute m msgs) (S.toList xs)))
                           ys'' = S.union ys ys'
                       in if null xs' then (S.empty,ys'') 
                          else loop m (xs',ys'') msgs
                                                                                             
continue :: [String] -> Bool
continue [] = False
continue (x:xs) = '{' `elem` x || continue xs


format :: String -> [String]
format s = map fmt (splitOn " | " s)
           where fmt r | r == "\"a\"" = "a"
                       | r == "\"b\"" = "b"
                       | otherwise = foldl f "" (splitOn " " r)
                                     where f s u = s++"{"++u++"}"

substitute :: M.Map Int [String] -> [String] -> String -> [String]
                        -- u=the known start of the word (ie 'a's and 'b's)
                        -- v=the remainder (excluding the first {n} if exists)
                        -- w=current lookup value (derived from first {n} found in word if exists)
                        -- b=true if we have a current lookup value
substitute m msgs xs =   let (u,v,w,_) = foldl process ([],[],[],False) xs
                                        where process (h,t,i,b) x | x `elem` "ab" = (h++[x],t,i,b)
                                                                  | x `elem` "{" && null i = (h,t,i,b)
                                                                  | x `elem` "0123456789" && null t = (h,t,i++[x],b)
                                                                  | x `elem` "}" && not b = (h,t,i,True)
                                                                  | otherwise = (h,t++[x],i,b)
                        in if not (checkMsg u v msgs) then [] 
                           else if null w then [u ++ v]
                           else case M.lookup (read w) m of
                                    Nothing    -> error "Invalid lookup"
                                    Just y     -> case length y of
                                                    1  -> substitute m msgs (u ++ head y ++ v)
                                                    2  -> [u ++ head y ++ v] ++ [u ++ last y ++ v]

--check whether (start of) word being generated exists in the (start of the) messages to be checked
--every {n} will eventually become an 'a' or 'b' so we can include each '{' in length of word
checkMsg :: String -> String -> [String] -> Bool
checkMsg [] _ _ = True
checkMsg u v msgs = let l = [length x | x <- msgs, u == take (length u) x]
                    in if null l then False else (length u + (length (filter (== '{') v))) <= maximum l
