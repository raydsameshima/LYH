module Codes where

-- chapter 1
lucky 7 = "Lucky number seven!"
lucky _ = "Oh, sorry. You are out of luck..."

-- chapter 2
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

fac1,fac2 :: Integer -> Integer
fac1 n = product [1..n]
fac2 0 = 1
fac2 n = n * (fac2 (n-1))

-- chapter 3
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
