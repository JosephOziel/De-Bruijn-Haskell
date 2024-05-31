module Main where

data Expr = 
    Var Int
    | Lambda Expr
    | Apply Expr Expr

main :: IO ()
main = putStrLn "Hello, Haskell!"
