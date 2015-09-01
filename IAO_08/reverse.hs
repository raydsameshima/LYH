-- reverse.hs
-- chapter 8

main = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

{-
Prelude> :type return ()
return () :: Monad m => m ()

Prelude> :type words
words :: String -> [String]
Prelude> words "hey it's a nice sturday, isn't it?"
["hey","it's","a","nice","sturday,","isn't","it?"]

Prelude> :type unwords 
unwords :: [String] -> String
-}
