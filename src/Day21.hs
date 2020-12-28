module Day21
where

import Data.List.Split
import Data.List

type Food = ([String], [String])
type Ingredient = (String, [String])

partOne :: IO ()
partOne = do
    food <- map format . map (splitOn "(") . lines <$> readFile "Inputs/21_input.txt"
    let ingredients = map head (group (sort ( concatMap fst food)))
    let (food', algs') = loop food (map (\(a,[b]) -> (a,b)) . filter ((==1).length.snd) $ getIngredToAllergenMatch food)
    let algIngreds = map head . group .sort $ map (\([x],_) -> x) food'
    let noalgIngreds = filter (`notElem` algIngreds) ingredients
    display . map snd . map head . group . sort $ map (\([x],[y]) -> (y,x)) food'
    where
        format [i,al] = (filter (not . null) (splitOn " " i), splitOn ", " (init (drop 9 al)))

display :: [String] -> IO ()
display [] = putStr ""
display (x:xs) = do
    putStr (x++",")
    display xs

singletonAllergens xs = map head . group . sort . concatMap snd . filter ((>1).length.fst) $ filter ((==1).length.snd) xs

getIngredientsByAllergen fd alg = map fst $ filter ((alg `elem`) . snd) fd

intersectFoods fd alg = (alg, foldl' intersect (head (getIngredientsByAllergen fd alg)) (getIngredientsByAllergen fd alg))

getIngredToAllergenMatch fd = map (intersectFoods fd) (singletonAllergens fd)



--loop :: [([String], [String])] -> [(String, String)] -> 
loop food [] = (food,map (\(a,[b]) -> (a,b)) . filter ((==1).length.snd) $ getIngredToAllergenMatch food)
loop food ((alg,ingred):algs) = let food' = [if alg `elem` y && length y > 1 
                                              then (delete ingred x, delete alg y) 
                                              else if y == [alg] then ([ingred], y) 
                                                   else if ingred `elem` x then (delete ingred x, y) else (x,y) | (x,y) <- food]
                                    algs' = map (\(a,[b]) -> (a,b)) . filter ((>1).length.fst) . filter ((==1).length.snd) $ getIngredToAllergenMatch food'
                                in loop food' algs'


