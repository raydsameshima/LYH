-- random_string.hs

import System.Random

main = do
  gen <- getStdGen
  putStrLn $ take 20 $ randomRs ('a', 'z') gen
  -- gen' <- getStdGen
  gen' <- newStdGen
  putStrLn $ take 20 $ randomRs ('a', 'z') gen'
