module Main where

import Prelude

import Data.Either
import Data.Functor

import Control.Monad

import qualified Text.Parsec as Parsec
import Text.Parsec hiding (parse, runParser)

data Expr = 
    Var Int
    | ID Char
    | Lam Expr
    | App Expr Expr
    | Def Char Expr
    deriving (Show)

runParser str = Parsec.runParser (expr <* eof) () "" str

parse str = case runParser str of
    (Right r)   -> r
    (Left e)    -> error $ show e

-- TODO: fix that program can start with whitespace
expr :: (Stream s m Char) => ParsecT s u m Expr
expr = chainl1 (nonApp <* spaces) (pure App) <?> "expr" where
    num = do
        (many1 digit) <?> "num"
    sym = do 
        (alphaNum) <?> "sym"
    var = (Var . read <$> num) <?> "var"
    name = (ID <$> sym) <?> "id"
    lam = do
        body <- char '\\' >> spaces >> expr
        (return (Lam body)) <?> "lam"
    def = do
        char ':' >> spaces
        name <- sym
        spaces 
        body <- brackets expr
        (return (Def name body)) <?> "def"
    nonApp = (def <|> name <|> parens expr <|> lam <|> var) <?> "non-app expr"
    parens = between
        (char '(' >> spaces)
        (char ')' >> spaces)
    brackets = between
        (char '{' >> spaces)
        (char '}' >> spaces)

main :: IO ()
main = putStrLn $ show (parse ": 9 { \\(a 2) }")
