module Main where

import Data.List         (group)
import Data.Set          (toList, fromList)
import Control.Arrow

f :: [Char] -> [(Int, [Char])]
f x = zip (length <$> group x) (toSet <$> group x)
  where
    toSet = toList . fromList

main :: IO ()
main = do
  let foo = zip (length <$> group "aaaabbbcca") (toList . fromList <$> group "aaaabbbcca")
      bar = (\x -> zip (length <$> x) (toList . fromList <$> x)) $ group "aaaabbbcca"
      baz = (\ (x,y) -> zip x y ) $ (length <$>) &&& (toList . fromList <$>) $ group "aaaabbbcca"
  putStrLn "Hello, Haskell!"
