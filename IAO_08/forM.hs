-- forM.hs

import Control.Monad

main = do
  colors <- forM [1..4] (\a -> do
    putStrLn $ "Which color do you associate with the number "
               ++ show a ++ "?"
    color <- getLine
    return color)
  putStrLn "The colors that you associate with 1..4 are: "
  -- mapM putStrLn colors
  mapM_ putStrLn colors
