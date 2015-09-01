-- when.hs
-- chapter 8

import Control.Monad

main = do
  input <- getLine
  when (input == "SWORDFISH") $ do
    putStrLn input

{- Above code is equivalent to

  input <- getLine
  if (input == "SWORDFISH")
    then putStrLn input
    else return ()
-}

{-
Prelude> :t Control.Monad.when
Control.Monad.when :: Monad m => Bool -> m () -> m ()
-}
