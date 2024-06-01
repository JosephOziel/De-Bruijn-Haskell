module Main where

import Prelude

import Data.Either
import Data.Functor

import Control.Monad

import qualified Text.Parsec as Parsec
import Text.Parsec hiding (parse, runParser)

data Expr = 
    Var Int
    | Lam Expr
    | App Expr Expr

runParser str = Parsec.runParser (expr <* eof) () "" str

parse str = case runParser str of
    (Right r)   -> r
    (Left e)    -> error $ show e

expr :: (Stream s m Char) => ParsecT s u m Expr
expr = chainl1 (nonApp <* spaces) (pure App) <?> "expr" where
    num = do
        (many1 digit) <?> "name"
    var = (Var <$> num) <?> "var"
    lam = do
        ns <- char '\\' >> spaces >> sepEndBy1 name spaces
        e <- char '.' >> spaces >> expr
        (return (foldl1 (.) (fmap Lam ns) e)) <?> "lam"
    nonApp = (parens expr <|> lam <|> var) <?> "non-app expr"
    parens = between
        (char '(' >> spaces)
        (char ')' >> spaces)

main :: IO ()
main = putStrLn "Hello, Connard!"
