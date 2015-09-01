-- handle.hs

import System.IO

main = do
  contents <- readFile "girlfriend.txt"
  putStr contents

{-
Because we don't get a handle with which to identify our file, we can't close it manually, so Haskell does that or us when use readFile.

-}
