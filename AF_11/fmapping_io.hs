-- fmaping_io.hs

import Data.Char
import Data.List

main = do
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line

{-
$ runhaskell fmapping_io.hs 
Hello Haskell
L-L-E-K-S-A-H- -O-L-L-E-H

hask
HASK
KSAH
K-S-A-H
-}
