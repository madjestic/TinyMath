module Main where

import Linear.V2
import Linear.Matrix
import Data.Massiv.Array
import Data.List         (group)
import Data.Set          (toList, fromList)

import qualified TinyMath

foo = zip (fmap length (group "aaaabbbcca")) (fmap (toList . fromList) (group "aaaabbbcca"))

mA = V2 (V2  4 3) (V2  6 9)
mB = V2 (V2 (-2) 9) (V2 (-5) 2)

x = undefined

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ identityMatrix (Sz1 5)
--  MyLib.someFunc
