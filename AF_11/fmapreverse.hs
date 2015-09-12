-- fmapreverse.hs

main = do
  line <- fmap reverse getLine
  -- line' <- getLine
  -- let line = reverse line'
  putStrLn $ "You said " ++ line ++ " backwards!"
  putStrLn $ "Yes, you really said " ++ line ++ " backwards!"
