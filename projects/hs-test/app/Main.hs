module Main where

import Data.Char

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"
main = do
  contents <- getContents
  putStr (map toUpper contents)
  from
