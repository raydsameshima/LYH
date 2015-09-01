-- capslocker2.hs
-- chapter 9

{-
import Control.Monad -- for forever
import Data.Char -- for toUpper

main = forever $ do
  l <- getLine
  putStrLn $ map toUpper l
-}

import Data.Char (toUpper)

main = do
  contents <- getContents
  putStr $ map toUpper contents

