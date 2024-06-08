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
    var = (Var . read <$> num) <?> "var"
    lam = do
        body <- char '\\' >> spaces >> expr
        (return (Lam body)) <?> "lam"
    nonApp = (var <|> parens expr <|> lam) <?> "non-app expr"
    parens = between
        (char '(' >> spaces)
        (char ')' >> spaces)

-- `subst i v e` -- replace all occurences of `Var i` with `v` in `e`. recall
-- how the index of a variable varies with how nested it is, and keep track of
-- that!
subst :: Int -> Expr -> Expr -> Expr
subst i v (Var n)
  | i == n    = v
  | otherwise = Var n
subst i v (Lam m) = Lam (subst (i+1) (shift 1 0 v) m)
subst i v (App f x) = App (subst i v f) (subst i v x)

shift :: Int -> Int -> Expr -> Expr
shift d c (Var k)
  | k < c    = Var k
  | k >= c   = Var (k+d)
shift d c (Lam m) = Lam (shift d (c+1) m)
shift d c (App f x) = App (shift d c f) (shift d c x)

eval :: Expr -> Expr
-- β-reduction; perform substitution
eval (App f x) = case eval f of
    Lam m -> eval $ subst 0 x m
    App f' x' -> eval (App f' x') 
    Var n -> Var n
-- leave everything else alone
eval e = e

main :: IO ()
main = putStrLn $ show $ eval $ parse "(\\\\1 2) 1"
-- (\\0 0)(\\0 0) -> infinite loop WORKS
-- (\\\\0)(\\0) -> \\1 NO WORKS
-- (\\\\1) 3 -> \\4 WORKS
-- (\\\\1 2) 1 -> \\2 1 NO WORKS
