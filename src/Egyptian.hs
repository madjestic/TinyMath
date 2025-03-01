{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Egyptian where

import Foreign.C.Types
import Foreign.Marshal.Array (withArray)
--import Unsafe.Coerce
import qualified Language.C.Inline as C
import Foreign.Ptr (Ptr)  

C.include "<stdbool.h>"
C.include "halff.h"
C.include "mult.h"
C.include "length.h"
C.verbatim "int half1(int n) { return n >> 1; }"
C.verbatim "bool odd1(int n) { return n & 0x1; }"  

foreign import ccall unsafe "odd1"  c_odd1  :: CInt -> CBool
foreign import ccall unsafe "half1" c_half1 :: CInt -> CInt
foreign import ccall unsafe "halff" c_halff :: CInt -> CInt
foreign import ccall unsafe "mult_acc4" c_mult_acc4 :: CInt -> CInt -> CInt -> CInt
foreign import ccall unsafe "length" c_length :: Ptr CInt -> CInt

-- 5 * 3
multiplyBy_15 :: CInt -> CInt
multiplyBy_15 a = (c + c) + b
  where
    b = (a + a) + a
    c = b + b

-- 4 * 3
multiplyBy_12  :: CInt -> CInt
multiplyBy_12 a = c + c
    where    
        b = (a + a) + a
        c = b + b

multiply :: CInt -> CInt -> CInt
multiply 1 a = a
multiply n a = c_mult_acc4 a (n-1) a

length' :: [CInt] -> IO CInt
length' arr = do
  --let arr' = [10, 20, 30, 40, -1] :: [CInt]
  withArray arr $ \ptr -> do
    return $ c_length ptr
