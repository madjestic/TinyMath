module Numeric.Tiny.Random where

import Data.ByteString.Char8 as Ch8 (pack, unpack)
import System.Random
import Data.Locator

import Data.UUID ( UUID, toWords )

genSeedUUID :: Int -> UUID
genSeedUUID seed =
  let
      g0      = mkStdGen (fromIntegral seed) -- RNG from seed
      (u1, _) = random g0
  in u1

-- | Generate a UUID, based on FilePath
-- | e.g. fromUUID $ encodeStringUUID "./projects/.temp1"
-- | > 2836415114
-- | TODO: rename encodeStringUUID to hashStringToUUID
encodeStringUUID :: String -> UUID
encodeStringUUID x = genSeedUUID . fromInteger . fromBase62 . unpack . hashStringToBase62 6 $ Ch8.pack x

--fromUUID :: UUID -> GLuint
fromUUID = read . concatMap show . (\ (x,y,z,w)-> fmap toInteger [x,y,z,w]) . toWords

rand :: UUID -> Int
rand x = fromIntegral . fromUUID . encodeStringUUID $ show x

toDouble :: Int -> Int -> Double
toDouble whole frac =
  whole' + ((\x -> if x == 0 then 1 else x) (signum whole') * frac')
  where
    whole' = fromIntegral whole
    frac'  = fromIntegral frac / 10^(length . show . abs $ frac)

rand01 :: Int -> Double
rand01 x = --toDouble ((\x' -> if x' == 0 then 1 else x') (signum x)) (frac)
  toDouble 0 (frac)
  where
    frac = fromIntegral . rand $ genSeedUUID x

rand01' :: Int -> Double
rand01' x =
  fromIntegral frac / 10 ^ numDigits
  where
    frac      = abs (rand $ genSeedUUID x)
    numDigits = length (show frac) :: Int
  

randInt :: Int -> Int
randInt n = round $ 10 * rand01 n
