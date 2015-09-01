-- removetodo.hs

import System.IO -- for openTempFile
import System.Directory
import Data.List

main = do
  contents <- readFile "todo.txt"
  let todoTasks = lines contents
      numberedTasks = zipWith withNumber [0..] todoTasks
        where withNumber n line = show n ++ " - " ++ line
                                
  putStrLn "Thee are your TO-DO items:"
  mapM_ putStrLn numberedTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  (tempName, tempHandle) <- openTempFile "." "temp"
  -- openFile :: FilePath -> IOMode -> IO Handle
  hPutStr tempHandle newTodoItems
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"
  -- renameFile :: FilePath -> FilePath -> IO ()
