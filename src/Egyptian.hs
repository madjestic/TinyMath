{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Egyptian where

import Foreign.C.Types
import Unsafe.Coerce
import qualified Language.C.Inline as C

C.include "<stdbool.h>"
C.include "halff.h"
C.verbatim "int half(int n) { return n >> 1; }"
C.verbatim "bool odd(int n) { return n & 0x1; }"  

foreign import ccall unsafe "odd"   c_odd   :: CInt -> CBool
foreign import ccall unsafe "half"  c_half  :: CInt -> CInt
foreign import ccall unsafe "halff" c_halff :: CInt -> CInt

multiply :: CInt -> CInt -> CInt
multiply 1 a = a
multiply n a = if unsafeCoerce $ c_odd n
  then result + a
  else result
       where
         result = multiply (c_halff n) (a + a)
