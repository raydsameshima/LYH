-- let.hs
-- chapter 8

import Data.Char(toUpper)

main = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName  = map toUpper lastName
  putStrLn $ "hey " ++ bigFirstName ++ " "
                    ++ bigLastName
                    ++ ", how are you?"

{-
Whereas <- is used to perform I/O actions and bind their results to names, let is used when we just want to give names to normal values inside I/O actions.
It's similar to the let syntax in the list comprehensions in page 47:

calcBmis xs = [bmi | (weight, height) <- xs, let bmi = weight / height^2]

-}
