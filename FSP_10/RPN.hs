-- RPN.hs
-- Chapter 10 Functionally Solving Problems
-- Reverse Polish Notation Calculator
-- ... the operator comes after the numbers, rather than being sandwiched between them.

module RPN where

-- 10 4 3 + 2 * -
-- 10 7 2 * -
-- 10 14 -
-- -4

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:ys) "*" = (y*x):ys
        foldingFunction (x:y:ys) "+" = (y+x):ys
        foldingFunction (x:y:ys) "-" = (y-x):ys
        foldingFunction (x:y:ys) "/" = (y/x):ys
        foldingFunction (x:y:ys) "^" = (y**x):ys
          -- (^) :: (Num a, Integral b) => a -> b -> a
          -- (**) :: Floating a => a -> a -> a
        foldingFunction (x:xs) "ln" = (log x):xs
        foldingFunction xs "sum" = [sum xs]
        foldingFunction xs numString = read numString:xs
{-
This RPN calculation solution is not really fault tolerant.
When given input that doesn't make sense, it might result in a runtime error.
But don't worry, you'll learn how to make this function more robust in Ch. 14.
-}
