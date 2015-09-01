HR_04.lhs

Chapter 04: Hello Recursion!

> module HR_04 where

Recursive function calls itself.

> maximum' :: (Ord a) => [a] -> a
> maximum' [] = error "empty list!"
> maximum' [x] = x
> maximum' (x:xs) 
>   | x > (maximum' xs) = x
>   | otherwise         = maximum' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:(replicate (n-1) x)

Above implement is not good for negative n.
*HR_04> replicate' (-3) 5
[5]

> replicate' :: Int -> a -> [a]
> replicate' n x
>   | n <= 0    = []
>   | otherwise = x:(replicate' (n-1) x)

> take' :: (Num i, Ord i) => i -> [a] -> [a]
> take' _ []    = []
> take' n (x:xs)
>   | n <= 0    = []
>   | otherwise = x:(take' (n-1) xs)  

In GHC, take are given by

take n _      | n <= 0 =  []
take _ []              =  []
take n (x:xs)          =  x : take (n-1) xs

> reverse' :: [a] -> [a]
> reverse' [] = []
> reverse' (x:xs) = (reverse' xs) ++ [x]

> repeat' :: a -> [a]
> repeat' x = x : repeat' x

> zip' :: [a] -> [b] -> [(a,b)]
> zip' _  [] = []
> zip' [] _  = []
> zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)

> isElement :: (Eq a) => a -> [a] -> Bool
> isElement _ [] = False
> isElement x (y:ys)
>   | x == y    = True
>   | otherwise = isElement x ys

Quick, Sort!

> quickSort :: (Ord a) => [a] -> [a]
> quickSort [] = []
> quickSort (x:xs) =
>   let smaller = [a | a <- xs, a <= x]
>       larger  = [a | a <- xs, a >  x]
>   in quickSort smaller ++ [x] ++ quickSort larger

