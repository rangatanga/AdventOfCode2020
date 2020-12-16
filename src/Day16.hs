module Day16
where


import Data.List.Split (linesBy, splitOn)
import Data.List

partOne :: IO ()
partOne = do
    x <- linesBy (=="") . lines <$> readFile "Inputs/16_input.txt"
    print (sum (validateTickets (formatRules (head x)) (formatTickets (last x))))

partTwo :: IO ()
partTwo = do
    x <- linesBy (=="") . lines <$> readFile "Inputs/16_input.txt"
    let rules = formatRules (head x)
    let validNearby = getValidTickets (rules) (formatTickets (last x))
    let validNearbyT = transpose validNearby
    let z = foldl checkRules [] rules
            where checkRules out r = let (_,x) = foldl eval (0,[]) validNearbyT
                                             where eval (pos,xs) vals = if checkRule r vals then (pos+1, xs++[pos]) else (pos+1, xs)
                                     in out++[x]
    let z' = concat (take 6 (processRulesToField z))
    let myTicket = head (formatTickets (x!!1))
    let res =  product [myTicket!!x | x <- z']
    print res


formatRules :: [String] -> [[(Int,Int)]]
formatRules = foldl fmt []
              where fmt rules x = let y = splitOn " " x
                                      r1 = splitOn "-" ((reverse y)!!2)
                                      r2 = splitOn "-" (last y)
                                  in rules ++ [[(read (head r1) :: Int, read (last r1) :: Int), (read (head r2) :: Int, read (last r2) :: Int)]]

formatTickets :: [String] -> [[Int]]
formatTickets = foldl fmt [] 
                where fmt tckts x = if "nearby" `isPrefixOf` x || "your" `isPrefixOf` x then tckts
                                    else let y = splitOn "," x
                                             y' = foldl cnv [] y
                                                  where cnv xs y = xs ++ [read y :: Int]
                                         in tckts ++ [y']

getValidTickets :: [[(Int,Int)]] -> [[Int]] -> [[Int]]
getValidTickets rules = foldl eval []
                        where eval valid tck = if null (validateTickets rules [tck]) then valid++[tck] else valid

validateTickets :: [[(Int,Int)]] -> [[Int]] -> [Int]
validateTickets rules = foldl process []
                        where process failures tck = failures ++ (checkField rules tck)

checkField :: [[(Int,Int)]] -> [Int] -> [Int]
checkField rules = foldl eval []
                   where eval fails x = if isValid x rules then fails else fails ++ [x]


isValid :: Int -> [[(Int,Int)]] -> Bool
isValid x = foldl eval False
                  where eval b r = b || (x >= fst (head r) && x <= snd (last r))

checkRule :: [(Int,Int)] -> [Int] -> Bool
checkRule r = foldl eval True
              where eval b x = b && ((x >= fst (head r) && x <= snd (head r)) || (x >= fst (last r) && x <= snd (last r)))

processRulesToField :: [[Int]] -> [[Int]]
processRulesToField xs = if maximum (map length xs) > 1 
                        then let singles = concat [x | x <- xs, length x == 1 ]
                             in processRulesToField (removeSingles singles xs)
                        else xs

removeSingles :: [Int] -> [[Int]] -> [[Int]]
removeSingles singles = foldl remove []
                        where remove ys x = if length x == 1 then ys++[x]
                                            else ys++[[z | z <- x, z `notElem` singles]]
