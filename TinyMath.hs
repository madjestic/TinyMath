module TinyMath where

-- | isPrime1 n k is just an interface function for isPrime
isPrime1 :: RealFrac a => a -> Bool
isPrime1 n = isPrime n 2

-- | isPrime needs an index k (since we can't use variables)
isPrime :: RealFrac a => a -> a -> Bool
isPrime n k
        | k < 2                                    = error "k must be >= 2"
        | n /= k && (n `isDivisableBy` k) == True  = False
        | n > k && (n `isDivisableBy` k)  == False = isPrime n (k+1)
        | n == k                                   = True


primesLessThan :: (Enum a, Integral b, RealFrac a) => a -> [b]
primesLessThan n 
               | n <= 2 = error "argument must be > 2"
               | otherwise = map round [x | x <- [2..n], isPrime1 x]


-- | factorize1 function is an interface to factorize function
factorize1 :: (Integral b, RealFrac a) => a -> [b]
factorize1 n = map round $ factorize n 2

-- | factorize requires an index k (since we can't use variables)
factorize :: RealFrac a => a -> a -> [a]
factorize n k
          | k >= n = n : []
          | n `isDivisableBy` k          = k : factorize (n/k) k
          | n `isDivisableBy` k == False = factorize n (k+1)


isDivisableBy :: RealFrac a => a -> a -> Bool
isDivisableBy n m = let fullNumber = fromInteger $ round (n/m)
                    in (n/m) == fullNumber
