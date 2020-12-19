module Day18
where

--import Data.List.Split ( splitOn )
import Data.List
import Data.Char
import Control.Applicative

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char 
item = P (\inp -> case inp of
            []      -> []
            (x:xs)  -> [(x,xs)])

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                            []          -> []
                            [(v,out)]   -> [(g v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v,inp)])
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                            []          -> []
                            [(g,out)]   -> parse (fmap g px) out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                            []          -> []
                            [(v,out)]  -> parse (f v) out)

instance Alternative Parser where
    --empty :: Parser a
    empty = P (\inp -> [])
    --(<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                            []          -> parse q inp
                            [(v,out)]   -> [(v,out)])


sat :: (Char -> Bool) -> Parser Char 
sat p = do  x <- item
            if p x then return x else empty 

digit :: Parser Char 
digit = sat isDigit

char :: Char -> Parser Char 
char x = sat (== x)

string :: String -> Parser String
string []       = return []
string (x:xs)   = do    char x
                        string xs
                        return (x:xs)

letter :: Parser Char 
letter = sat isAlpha

nat :: Parser Int 
nat = do    xs <- some digit
            return (read xs)

space :: Parser ()
space = do  many (sat isSpace)
            return ()

token :: Parser a -> Parser a
token p = do    space
                v <- p
                space
                return v

integer :: Parser Int 
integer = token nat

natural :: Parser Int 
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)



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

exprop :: Parser (Int, Char)
exprop = do 
  f <- factor
  do
    symbol "+"
    return (f, '+')
    <|> do
      symbol "*"
      return (f, '*')

expr :: Parser Int 
expr =  do  es <- many exprop
            f <- factor
            return (fst (foldl1 op (es ++ [(f,' ')])))
            where op (vl, sl) (vr, sr) = case sl of
                                            '+' -> (vl + vr, sr)
                                            '*' -> (vl * vr, sr)
expr2 :: Parser Int 
expr2 = do  t <- term
            do symbol "*"
               e <- expr2
               return (t * e)
             <|> return t

term :: Parser Int 
term = do f <- factor2
          do  symbol "+"
              t <- term
              return (f + t)
           <|> return f


factor :: Parser Int 
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> natural

factor2 :: Parser Int 
factor2 = do symbol "("
             e <- expr2
             symbol ")"
             return e
           <|> natural

eval :: Parser Int -> String -> Int
eval p xs = case (parse p xs) of
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


