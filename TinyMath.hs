module TinyMath where

-- |
-- | Prime Factorisation
-- |

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
                    
-- |
-- | Prime spiral
-- |

mono1 :: (Enum a, Num a) => Int -> [a]
mono1 n = let mon = concatMap (\x -> [x,x]) (take n [1..])
          in mon


mono2 :: Num a => [t] -> [a]
mono2 list = concat $ rowPairs list
    where
        rowPairs [] = []
        rowPairs (_:_:xs) = [1,0] : rowPairs xs


-- mono3 list p = concat $ rowPairs list p
--    where
--        rowPairs [] _ = []
--        rowPairs (_:_:xs) (x,y) = [(x+1,y),(x+1,y+1)] : rowPairs' xs (x,y)
--        rowPairs' [] _ = []
--        rowPairs' (_:_:xs) (x,y) = [(x-1,y),(x-1,y-1)] : rowPairs xs (x,y)

-- mono4 [] _ = []
-- mono4 (_:_:xs) (x,y) = [(x+1,y),(x+1,y+1)] : mono4 xs (x,y)

-- mono5 _ 0 = []
-- mono5 f k = f : mono5 f (k-1)

-- | in <- [1,1,2,2,3,3,4,4,...n,n] p@(x,y)
-- mono6 list p = concat $ walkSpiral list p

--walkSpiral (stepsRight:stepsUp:stepsLeft:stepsDown:xs) p = walkRight stepsRight p

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


offset :: (Eq t, Num t, Num t2, Num t1) => (t1, t2) -> (Direction, t) -> [(t1, t2)]
offset _ (_,0) = []
offset (x,y) (direction, steps)
         | direction == Right' = stepRight : offset stepRight (Right',(steps - 1))
         | direction == Up     = stepUp    : offset stepUp    (Up    ,(steps - 1))
         | direction == Left'  = stepLeft  : offset stepLeft  (Left' ,(steps - 1))
         | direction == Down   = stepDown  : offset stepDown  (Down  ,(steps - 1))
         | otherwise = error "foobar"
                   where 
                       stepRight = (x+1,y)
                       stepUp    = (x,y+1)
                       stepLeft  = (x-1,y)
                       stepDown  = (x,y-1)
                       
--traverse 

-- (+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
-- (+) (x1,y1) (x2,y2) = (x1+x2,y1+y2)

instance (Num a, Num b) => Num (a, b) where
         (x1,y1) + (x2,y2) = (x1+x2, y1+y2)
         (x1,y1) * (x2,y2) = (x1*x2, y1*y2)



directions :: [t] -> [Direction]
directions xs = concat $ directions' xs
           where                      
                 directions' [] = []
                 directions' (_:_:_:_:xs) = [Right', Up, Left', Down] : directions' xs
