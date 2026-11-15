module Main where

import System.Random (randomRIO)

import Numeric.Tiny.Random

main :: IO ()
main = do
  -- TODO: read a random number
  x <- randomRIO (1, 1000000) :: IO Int
  print (rand01 x)
  putStrLn $ "How much is 2 + 2 = ?"
  x' <- getLine
  putStrLn $ show ((read x' :: Int) == (2 + 2))
