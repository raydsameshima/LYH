-- returntest.hs
-- chapter 8

main = do 
  return ()
  return "hahaha"
  line <- getLine
  return "BLAH BLAH BLAH"
  return 4
  putStrLn line


