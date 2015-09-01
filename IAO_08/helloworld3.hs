-- helloworld3.hs
-- chapter 8

main = do
  _ <- putStrLn "Hello, what's your neme?"
  name <- getLine
  putStrLn $ "Hey " ++ name ++ ", you rock!"
