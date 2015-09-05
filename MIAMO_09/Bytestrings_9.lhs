Bytestrings_9.lhs

> module Bytestrings_9 where
>
> import qualified Data.ByteString.Lazy as B
> import qualified Data.ByteString      as S
>

B.pack :: [GHC.Word.Word8] -> B.ByteString
data GHC.Word.Word8 = GHC.Word.W8# GHC.Prim.Word#
    -- Defined in ‘GHC.Word’
    instance Bounded GHC.Word.Word8 -- Defined in ‘GHC.Word’
    instance Enum GHC.Word.Word8 -- Defined in ‘GHC.Word’
    instance Eq GHC.Word.Word8 -- Defined in ‘GHC.Word’
    instance Integral GHC.Word.Word8 -- Defined in ‘GHC.Word’
    instance Num GHC.Word.Word8 -- Defined in ‘GHC.Word’
    instance Ord GHC.Word.Word8 -- Defined in ‘GHC.Word’
    instance Read GHC.Word.Word8 -- Defined in ‘GHC.Word’
    instance Real GHC.Word.Word8 -- Defined in ‘GHC.Word’
    instance Show GHC.Word.Word8 -- Defined in ‘GHC.Word’

Many times, you can convert a program that uses normal strings to a program that uses bytestrings just by doing the necessary imports and then putting the qualified module names in front of some functions.
Sometimes, you need to convert functions that you wrote to work on strings so that they work on bytestrings, but that's not hard.
 
