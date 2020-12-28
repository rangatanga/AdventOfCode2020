module Parser (Parser, parse, symbol, natural, integer, char, string, letter, letters, digit, item, sat, void)
where

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

letters :: Parser String 
letters = many (sat isAlpha)

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


void :: String -> Parser String
void [] = return []
void xs = return xs

