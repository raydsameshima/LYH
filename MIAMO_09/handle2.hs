-- handle2.hs

import System.IO
{-
main = do
  handle <- openFile "girlfriend.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
-}

main = do
  withFile "girlfriend.txt" ReadMode (\handle -> 
    do contents <- hGetContents handle
       putStrLn contents)
{-
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a

... if anything goes wrong while we're operating on our file, withFile makes sure that the file handle gets closed.

(\handle -> ...) is the function that takes a handle and returns an I/O action, and it's usually done like this, with a lambda.
It needs to take a function that returns an I/O action, rather than just taking an I/O action to do and then closing the file, because the I/O action that we would  pass to it wouldn't know on which file to operate.
This way, withFile opens the file and then passes the handle to the function we gave it.
It gets and I/O action back from that function and then makes an I/O action that's just like the original action, but it also makes sure that the file handle gets closed, even if something goes awry.
-}
