HOF_05.lhs

Chapter 05: Higher-Order Functions

> module HOF_05 where

The higher order functions can take functions as their parameters and/or retun values.

The Functional Programmer's Toolbox

> map' :: (a -> b) -> [a] -> [b]
> map' _ []     = []
> map' f (x:xs) = f x : map f xs

> filter' :: (a -> Bool) -> [a] -> [a]
> filter' _ [] = []
> filter' p (x:xs)
>   | p x       = x:(filter' p xs)
>   | otherwise = filter' p xs

There is no set rule for when to use map and filter versus using list comprehensions.

*HR_04> filter (<15) (filter even [1..20])
[2,4,6,8,10,12,14]
*HR_04> [x | x <- [1..20], x < 15, even x]
[2,4,6,8,10,12,14]

> quickSort' :: (Ord a) => [a] -> [a]
> quickSort' [] = []
> quickSort' (x:xs) = quickSort' smaller ++ [x] ++ quickSort' larger
>   where smaller = filter (<  x) xs
>         larger  = filter (>= x) xs

More Examples of map and filter
Let us find the largest number under 100000 that's divisible by 3829.

> largestDivisible =  head $ filter pre  [100000, 99999..]  
>   where pre x = x `mod` 3829 == 0

Find the sum of all odd squares that are smaller than 10000.

> oddSquareSum = sum [x | x <- [n^2 | n <- [1..]], odd x, x <= 10000]

This code does NOT terminate...

> oddSquareSum' = sum $ takeWhile (<= 10000) [x^2 | x <- [1..], odd x]

Collatz sequence/chain

> chain :: Integer -> [Integer]
> chain 1 = [1]
> chain n
>   | even n    = n:chain (n `div` 2) -- (n/2) does NOT work!
>   | otherwise = n:chain (3*n +1)

> numLongChains' :: Int -> Int
> numLongChains' n = length (filter isLong (map chain [1..100]))
>   where isLong xs = length xs > n

I fold you so
A fold takes a binary function, starting value(accumulator), and a list to fold up.
Folds are the recursive design pattern.
For a list [x,y,z],
  foldl f a [x,y,z] = foldl f (f a x) [y,z]
                    = foldl f (f (f a x) y) [z]
                    = foldl f (f (f (f a x) y) z) []
                    = f (f (f (f a x) y) z) 

> myFoldl, myFoldl' :: (a -> b -> a) -> a -> [b] -> a
> myFoldl f a []     = a
> myFoldl f a (b:bs) = myFoldl f (f a b) bs

> myFoldl' f a []     = a 
> myFoldl' f a (x:xs) = let a' = f a x
>                       in a' `seq` myFoldl' f a' xs -- ...it(seq) magically strict in its first argument.

> sum' :: Num a => [a] -> a
> -- sum' xs = foldl (\acc x -> acc + x) 0 xs
> -- sum' = foldl (+) 0 
> sum' = myFoldl' (+) 0 

Using myFoldl' is more speedy than myFold:
*HOF_05> sum' [1..1000000]
500000500000
(0.95 secs, 251,312,496 bytes)  <- using myFoldl
(0.64 secs, 266,872,264 bytes)  <- using myFoldl'

Right folds with foldr
For a list [x,y,z],
  foldr f a [x,y,z] = f x (foldr f a [y,z])
                    = ...
                    = f x (f y (f a z))

> myFoldr :: (a -> b -> b) -> b -> [a] -> b
> myFoldr f a [] = a
> myFoldr f a (x:xs) = f x (myFoldr f a xs)

> myMapWithoutFold :: (a -> b) -> [a] -> [b]
> myMapWithoutFold _ []     = []
> myMapWithoutFold f (x:xs) = (f x) : myMapWithoutFold f xs
  
> myMapl, myMapr :: (a -> b) -> [a] -> [b]
> myMapl f xs = myFoldr (\x acc -> f x : acc) [] xs
>
> myMapr f xs = myFoldl (\acc x -> acc ++ [f x]) [] xs

... the ++ function is much slower than :, so we usually use right folds when we're building up new lists from a list.
And using right folds, we can NOT treat infinite lists, never terminate:
*HOF_05> take 10 $ myMapl (+1) [1..]
[2,3,4,5,6,7,8,9,10,11]
(0.01 secs, 3090760 bytes)
*HOF_05> take 10 $ myMapr (+1) [1..]
^CInterrupted.

> isElem :: (Eq a) => a -> [a] -> Bool
> isElem y ys = foldr (\x acc -> if x == y then True
>                                          else acc) False ys 

*HOF_05> 'H' `isElem` "Haskell"
True

Some fold examples

> myReverse :: [a] -> [a]
> myReverse = myFoldl (\acc x -> x : acc) []
> -- myReverse = myFoldl (flip (:)) []

> myFilter :: (a -> Bool) -> [a] -> [a]
> myFilter p = myFoldr (\x acc -> if p x then x : acc
>                                        else acc) []

Folding infinite lists

> myAnd :: [Bool] -> Bool
> myAnd xs = myFoldr (&&) True xs

Scans

Point-Free Style
The preferred style is to use let bindings to give labels to intermediary results or to split the problem into subproblems that are easier for someone reading the code to understand.
