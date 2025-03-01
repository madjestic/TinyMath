module Main where

import Data.List         (group)
import Data.Set          (toList, fromList)
import Control.Arrow
import RealArithmetic

import Egyptian as Egyptian

f :: [Char] -> [(Int, [Char])]
f x = zip (length <$> group x) (toSet <$> group x)
  where
    toSet = toList . fromList

e' :: String -> Double
e' = eval . parse . lexer
-- | \> (e' "10 + 5") == (e' "5.5 + 9.5")
-- | > True

main :: IO ()
main = do
  let foo = zip (length <$> group "aaaabbbcca") (toList . fromList <$> group "aaaabbbcca")
      bar = (\x -> zip (length <$> x) (toList . fromList <$> x)) $ group "aaaabbbcca"
      baz = (\ (x,y) -> zip x y ) $ (length <$>) &&& (toList . fromList <$>) $ group "aaaabbbcca"
  --putStrLn "Hello, Haskell!"
  print $ multiply 1 2
  pure ()
