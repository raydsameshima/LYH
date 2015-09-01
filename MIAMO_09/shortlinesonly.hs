-- shortlinesonly.hs
{-
import Data.Char -- getContents
main = do
  contents <- getContents
  putStr $ shortLinesOnly contents
-}

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines

{-
Prelude> :type interact 
interact :: (String -> String) -> IO ()
-}

main = interact shortLinesOnly
