-- guess_the_number.hs

import System.Random
import Control.Monad(when)

main = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
  putStrLn "Which number in the range from 1 to 10 am I thinking of? "
  numberString <- getLine
  when (not $ null numberString) $ do
    let number = read numberString
    if randNumber == number
         then putStrLn "You are correct!"
         else putStrLn $ "Sorry, it was " ++ show randNumber
    askForNumber newGen

{-
If the user enters some input that read can't parse (like "haha"), our program will crash with an ugly error message:
  guess_the_number.hs: Prelude.read: no parse
If you don't want your program to crash on erronous input, use reads, which returns an empty list when it fails to read a string:
  read :: Read a => String -> a
  reads :: Read a => ReadS a
When it suceeds, it returns a singleton list with a tuple that has your desired value as one component and a string with what it didn't consume as the other.
-}
