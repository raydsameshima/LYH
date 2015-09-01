-- capslocker.hs
-- chapter 9


import Control.Monad (forever)
import Data.Char (toUpper)

main = forever $ do
  l <- getLine
  putStrLn $ map toUpper l

{-
import Data.Char

main = do
  contents <- getContents
  putStr $ map toUpper contents
-}
