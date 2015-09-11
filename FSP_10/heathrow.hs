-- heathrow.hs

import Data.List
import H2L

main = do
  contents <- getContents
  let threes = groupsOf 3 $ map read $ lines contents
      roadSystem = map (\[a,b,c] -> Section a b c) threes
      path = optimalPath roadSystem
      pathString = concat $ map (show . fst) path
      pathTime = sum $ map snd path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "Time taken: " ++ show pathTime
