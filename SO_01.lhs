SO_01.lhs

Chapter 01: Stating Out

> module SO_01 where

succ :: Enum a => a -> a

> conanO'Brien = "It's a-me, Conan O'Brien!"

conanO'Brien :: [Char]

(!!) :: [a] -> Int -> a
"Steve Buscemi" !! 6
'B'

cycle :: [a] -> [a]
*SO_01> take 10 $ cycle [1..3]
[1,2,3,1,2,3,1,2,3,1]

*SO_01> take 10 $ repeat 'q'
"qqqqqqqqqq"

*SO_01> :type replicate
replicate :: Int -> a -> [a]
*SO_01> replicate 10 3
[3,3,3,3,3,3,3,3,3,3]

List complehention

> boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

*SO_01> :type boomBangs
boomBangs :: Integral a => [a] -> [[Char]]
O_01> boomBangs [1..13]
["BOOM!","BOOM!","BOOM!","BOOM!","BOOM!","BANG!","BANG!"]

*SO_01> :type zip
zip :: [a] -> [b] -> [(a, b)]
*SO_01> zip [1..10] [1,1..]
[(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(10,1)]

Finding the Right Triangle

> triples = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b]]
> rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], c^2 == a^2 + b^2]
> rightTriangles' = [(a,b,c) | (a,b,c) <- triples, a^2+b^2 == c^2]
