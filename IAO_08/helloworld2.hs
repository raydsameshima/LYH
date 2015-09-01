-- helloworld2.hs
-- chapter 8

main = do
  putStrLn "Hello, what's your neme?"
  name <- getLine
  putStrLn $ "Hey " ++ name ++ ", you rock!"
