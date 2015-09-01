-- deletetodo.hs

import System.IO
import System.Directory
import Data.List
import Control.Exception

main = do
  contents <- readFile "todo.txt"
  let todoTasks = lines contents -- lines :: String -> [String]
      numberedTasks = zipWith withNum [0..] todoTasks
        where withNum n l = show n ++ " - " ++ l
  putStrLn "These are your TO-DO items:"
  mapM_ putStrLn numberedTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  -- bracketOnError :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
  -- To make sure our temporary file is cleaned up in case of a problem, we're going to use the bracketOnError function from Control.Exception.
  bracketOnError (openTempFile "." "temp") 
    (\(tempName, tempHandle) -> do -- if an error occurs
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do -- while things are going well
      hPutStr tempHandle newTodoItems
      hClose tempHandle
      removeFile "todo.txt"
      renameFile tempName "todo.txt")

{-
Instead of just using openTempFile normally, we use it with bracketOnError.
Next, we write what we want to happen if an error occurs; that is, we want to close the temporary handle and remove the temporary file. 
Finally, we write what we want to do with the temporary file while things are going well, and these lines are the same as they were before.
We write the new items, close the temporary handle, remove our current file, and rename the temporary file.
-}
