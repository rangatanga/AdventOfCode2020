module Day18
where

--import Data.List.Split ( splitOn )
import Data.List
import Data.Char
import Control.Applicative
import qualified Parser as P

{-
part one

expr    ::= [factor +] expr | [factor *] expr | factor
factor  :: = (expr) | nat
nat     ::= 0|1|2|...

part two
expr    ::= term * expr | term
term    ::= factor + term | factor
factor  :: = (expr) | nat
nat     ::= 0|1|2|...


-}

exprop :: P.Parser (Int, Char)
exprop = do 
  f <- factor
  do
    P.symbol "+"
    return (f, '+')
    <|> do
      P.symbol "*"
      return (f, '*')

expr :: P.Parser Int 
expr =  do  es <- many exprop
            f <- factor
            return (fst (foldl1 op (es ++ [(f,' ')])))
            where op (vl, sl) (vr, sr) = case sl of
                                            '+' -> (vl + vr, sr)
                                            '*' -> (vl * vr, sr)
expr2 :: P.Parser Int 
expr2 = do  t <- term
            do P.symbol "*"
               e <- expr2
               return (t * e)
             <|> return t

term :: P.Parser Int 
term = do f <- factor2
          do  P.symbol "+"
              t <- term
              return (f + t)
           <|> return f


factor :: P.Parser Int 
factor = do P.symbol "("
            e <- expr
            P.symbol ")"
            return e
         <|> P.natural

factor2 :: P.Parser Int 
factor2 = do P.symbol "("
             e <- expr2
             P.symbol ")"
             return e
           <|> P.natural

eval :: P.Parser Int -> String -> Int
eval p xs = case (P.parse p xs) of
            [(n,[])]    -> n
            [(_,out)]   -> error ("Unused input " ++ out)
            []          -> error "Invalid input"

partOne :: IO ()
partOne = do
    x <- lines <$> readFile "Inputs/18_input.txt"
    print (sum (map (eval expr) x))


partTwo :: IO ()
partTwo = do
    x <- lines <$> readFile "Inputs/18_input.txt"
    print (sum (map (eval expr2) x))


