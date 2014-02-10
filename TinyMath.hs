module TinyMath.TinyMath where

-- |
-- | Prime Factorisation
-- |

-- | isPrime1 n k is just an interface function for isPrime
isPrime1 :: RealFrac a => a -> Bool
isPrime1 n 
         | n == 1    = False
         | n == 2    = True
         | otherwise = isPrime n 2

-- | isPrime needs an index k (since we can't use variables)
isPrime :: RealFrac a => a -> a -> Bool
isPrime n k
        | k < 2                                    = error "k must be >= 2"
        | n /= k &&     (n `isDivisableBy` k)      = False
        | n  > k && not (n `isDivisableBy` k)      = isPrime n (k+1)
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
                    
-- |
-- | Prime spiral
-- |

double :: (Enum a, Num a) => Int -> [a]
double n = let mon = concatMap (\x -> [x,x]) (take n [1..])
          in mon


data Direction = Right'
               | Up
               | Left'
               | Down
               deriving Show

instance Eq Direction where
         Right' == Right' = True
         Up     == Up     = True
         Left'  == Left'  = True
         Down   == Down   = True
         _ == _ = False



offset :: (Eq t, Num t, Num t1, Num t2) => (Direction, t) -> [(t1, t2)]
offset (_,0) = []
offset (direction, steps)
         | direction == Right' =  (1,0) : offset (Right',(steps - 1))
         | direction == Up     =  (0,1) : offset (Up    ,(steps - 1))
         | direction == Left'  = (-1,0) : offset (Left' ,(steps - 1))
         | direction == Down   = (0,-1) : offset (Down  ,(steps - 1))

                       
traverse :: Num a => a -> [a] -> [a]
traverse _ []     = []
traverse p (x:xs) = p + x : traverse (p + x) xs



traverse1 :: (Num t, Num t1, Num t2) => t2 -> (t, t1) -> [(t, t1)] -> [(t, t1, t2)]
traverse1 _ _ []     = []
traverse1 k p (x:xs) = (\((a,b),c) -> (a,b,c)) (p + x, k) : traverse1 (k+1) (p + x) xs



walkSpiral :: (Num t, Num t1, Num t2) => (t, t1) -> Int -> [(t, t1, t2)]
walkSpiral p size = traverse1 1 p $ concat $ map offset $ zip (directions $ size') (size')
           where
              size' = double (size*2)

instance (Num a, Num b) => Num (a, b) where
         (a,b) + (c,d) = (a+c, b+d)


directions :: [t] -> [Direction]
directions xs = concat $ directions' xs
           where                      
                 directions' [] = []
                 directions' (_:_:_:_:xs) = [Right', Up, Left', Down] : directions' xs
