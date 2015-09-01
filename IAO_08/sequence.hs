-- sequence.hs
-- chapter 8

main = do 
  rs <- sequence [getLine, getLine, getLine]
  print rs

{-
Prelude> :type sequence
sequence :: Monad m => [m a] -> m [a]
-}
