-- todo.hs

import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add"    = add
dispatch "view"   = view
-- dispatch "remove" = remove
-- if you need more option, you can add here!

main = do
  (command:argList) <- getArgs
  dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith numbering [0..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith numbering [0..] todoTasks
  putStrLn "There are your TO-DO items:"
  mapM_ putStrLn numberedTasks    
  let number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      hPutStr tempHandle newTodoItems
      hClose tempHandle
      removeFile "todo.txt"
      renameFile tempName "todo.txt")

-- numbering :: Int -> String -> String
numbering n line = show n ++ " - " ++ line
