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
        foldingFunction xs numString = read numString:xs
