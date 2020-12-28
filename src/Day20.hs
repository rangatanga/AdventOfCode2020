module Day20
where

import Data.List.Split (linesBy)
import Data.List
import qualified Data.Set as S
import Control.Lens
import System.IO

type Tile = (Integer,Integer,String,String,String,String,[String]) --id,label,n,e,s,w,full tile (where label is different per variation of tile by rotation/flipping)
type Image = [[Tile]] --image is a square of dimension size x size
type ImagePt = (Int, Int)

size :: Int -- height/width of the Image
size = 12

emptyImage :: Image
emptyImage = replicate size (replicate size (0,0,[],[],[],[],[]))

monster :: [String]
monster = ["                  # " 
          ,"#    ##    ##    ###"
          ," #  #  #  #  #  #   "]

monsterPts :: [ImagePt]
monsterPts = [(0,0),(1,1),(4,1),(5,0),(6,0),(7,1),(10,1),(11,0),(12,0),(13,1),(16,1),(17,0),(18,0),(18,-1),(19,0)]

partOne :: IO ()
partOne = do
    x <- linesBy (=="") . lines <$> readFile "Inputs/20_input.txt"
    let allTiles =  (concatMap format x)
    let y = [t | t <- allTiles, t^._2 `elem` [1,2,3,4]]
    let z = fillImage emptyImage allTiles (0,0) y
    print (z)
    print ((z!!0!!0)^._1 * (z!!0!!(size-1))^._1 * (z!!(size-1)!!0)^._1 * (z!!(size-1)!!(size-1))^._1 )
    saveImage z


partTwo :: IO ()
partTwo = do
    partOne
    x <- lines <$> readFile "Inputs/20_input_part2.txt"
    print (checkForMonster x (0,1))
    let y = loop x 0
    print (y)

loop :: [String] -> Int -> Int
loop xs i = if i == 12 then error "no monsters found"
            else let xs' = concat (checkForMonster xs (0,1))
                     cnt = length (filter (== 'O') xs')
                 in if  cnt > 0 then length (filter (== '#') xs')
                    else if i==4 then let t = rotateTile xs 90
                                          xs'' = [t!!u | u <- [(length t)-1,(length t)-2..0]] 
                                      in loop xs'' (i+1)
                         else if i==8 then loop (map reverse (rotateTile xs 90)) (i+1)
                              else loop (rotateTile xs 90) (i+1)

checkForMonster :: [String] -> ImagePt -> [String]
checkForMonster xs (x,y) = if y == length xs-1 then xs
                           else let (x',y') = if x > length (head xs) - 18 then (0,y+1) else (x+1,y)
                                    found = foldl eval True monsterPts
                                            where eval b (u,v) = b && xs!!(y+v)!!(x+u) `elem` "#O"
                                in if found then checkForMonster (updateImage xs (x,y)) (x',y')
                                   else checkForMonster xs (x',y')

updateImage :: [String] -> ImagePt -> [String]
updateImage xs (x,y) = [[if (i-x,j-y) `elem` monsterPts then 'O' else xs!!j!!i | i <- [0..length (head xs)-1]] | j <- [0..length xs-1]]


fillImage :: Image -> [Tile] -> ImagePt -> [Tile] -> Image
fillImage _ _ _ [] = []
fillImage img allTiles (x,y) (t:ts) | x==size-1 && y==size-1 = updateImageTile img (x,y) t 
                                    | otherwise = let newImg = updateImageTile img (x,y) t 
                                                      (x',y') = if x==size-1 then (0,y+1) else (x+1,y)
                                                      candidates = [u | u <- allTiles, u^._1 `notElem` [(newImg!!j!!i)^._1 | i <- [0..(size-1)], j <- [0..(size-1)]]
                                                                                     , matchNeighbours newImg u (x',y')]
                                                  in if null candidates then fillImage img allTiles (x,y) ts
                                                     else let img' = fillImage newImg allTiles (x',y') candidates
                                                          in if null img' then fillImage img allTiles (x,y) ts
                                                             else img'

matchNeighbours :: Image -> Tile -> ImagePt -> Bool
matchNeighbours img t (x,y) = (y==0 || t^._3==(img!!(y-1)!!x)^._5 || (img!!(y-1)!!x)^._1==0)              --n edge matches
                                && (x==(size-1) || t^._4==(img!!y!!(x+1))^._6 || (img!!y!!(x+1))^._1==0)  --e edge matches
                                && (y==(size-1) || t^._5==(img!!(y+1)!!x)^._3 || (img!!(y+1)!!x)^._1==0)  --s edge matches
                                && (x==0 || t^._6==(img!!y!!(x-1))^._4 || (img!!y!!(x-1))^._1==0)         --w edge matches


updateImageTile :: Image -> ImagePt -> Tile -> Image
updateImageTile img (x,y) t = [[ if i==x && j==y then t else img!!j!!i | i <- [0..(size-1)]]| j <- [0..(size-1)]]

format :: [String] -> [Tile]
format xs = let i = read (init (drop 5 (head xs)))-- :: Integer 
                n   = head (drop 1 xs)
                --n'  = s                      -- the ' values are flipped along horiz
                n'' = reverse n              -- the '' values are flipped along vert
                s   = last xs                -- the n/s edges are read L to R, the e/w edges are read top to bottom
                --s'  = n
                s'' = reverse s
                e   = [last x | x <- drop 1 xs]
                --e'  = reverse e
                e'' = w
                w   = [head x | x <- drop 1 xs]
                --w'  = reverse w
                w'' = e
                t   = trimBorders (drop 1 xs)
                --t'  = [t!!u | u <- [(length t)-1,(length t)-2..0]]
                t'' = map reverse t
            in [(i,1,n,e,s,w,t), (i,2,reverse w,n,reverse e,s, rotateTile t 90), (i,3,reverse s,reverse w,reverse n,reverse e, rotateTile t 180), (i,4,e,reverse s,w,reverse n, rotateTile t 270)
                --,(i,5,n',e',s',w',t'), (i,6,reverse w',n',reverse e',s', rotateTile t' 90), (i,7,reverse s',reverse w',reverse n',reverse e', rotateTile t' 180), (i,8,e',reverse s',w',reverse n', rotateTile t' 270)
                ,(i,9,n'',e'',s'',w'',t''), (i,10,reverse w'',n'',reverse e'',s'', rotateTile t'' 90), (i,11,reverse s'',reverse w'',reverse n'',reverse e'', rotateTile t'' 180), (i,12,e'',reverse s'',w'',reverse n'', rotateTile t'' 270)]


saveImage :: Image -> IO ()
saveImage img = do
    let x = concat [getTileLines (img!!i) | i <- [0..(length img)-1]]
    outh <- openFile "Inputs/20_input_part2.txt" WriteMode
    writeList outh x
    hClose outh

writeList :: Handle -> [String] -> IO ()
writeList _ [] = print ""
writeList h (x:xs) = do
    hPutStrLn h x
    writeList h xs

rotateTile :: [String] -> Int -> [String]
rotateTile xs rot = case rot of
                        90 -> [[xs!!j!!i | j <- [(length xs)-1,(length xs)-2..0]]| i <- [0..(length xs)-1]]
                        180 -> rotateTile (rotateTile xs 90) 90 --[[xs!!j!!i | j <- [(length xs)-1,(length xs)-2..0]]| i <- [(length xs)-1,(length xs)-2..0]]
                        270 -> rotateTile (rotateTile xs 180) 90 -- [[xs!!j!!i | j <- [0..(length xs)-1]]| i <- [(length xs)-1,(length xs)-2..0]]
                        _ -> error "invalid rotation"

trimBorders :: [String] -> [String]
trimBorders xs = [init (drop 1 x) | x <- init (drop 1 xs)]

getTileLines :: [Tile] -> [String]
getTileLines ts = let tiles = [(ts!!i)^._7 | i <- [0..(length ts)-1]]
                  in [concat [tiles!!j!!i | j <- [0..(length ts)-1]] | i <- [0..(length (head tiles))-1]]