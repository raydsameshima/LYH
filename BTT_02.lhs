BTT_02.lhs

Chapter 02: Believe The Type

> module BTT_02 where

> removeNonUppercase :: [Char] -> [Char]
> removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

Integer

> factorial :: Integer -> Integer
> factorial 0 = 1
> factorial n = f' n 1
>   where f' 1 a = a -- using accumulator
>         f' n a = f' (n-1) (n*a)

Haskell type

*BTT_02> [minBound, maxBound] ::[Int]
[-9223372036854775808,9223372036854775807]
*BTT_02> 2^63 -1
9223372036854775807
*BTT_02> :type maxBound
maxBound :: Bounded a => a
Both minBound and maxBound are polymorphic constants (or constant functions).

*BTT_02> factorial 50
30414093201713378043612608166064768844377641568960512000000000000

*BTT_02> :type ()
() :: ()

The Read Type Class
The read function returns a value whose type is an instance of Read, but if we use that result in some way, it has no way of knowing which type.
To solve this problem, we can use type annotations.

*BTT_02> :t read
read :: Read a => String -> a
*BTT_02> read "1"

<interactive>:84:1:
    No instance for (Read a0) arising from a use of `read'
    The type variable `a0' is ambiguous
    Possible fix: add a type signature that fixes these type variable(s)
    Note: there are several potential instances:
      instance Read () -- Defined in `GHC.Read'
      instance (Read a, Read b) => Read (a, b) -- Defined in `GHC.Read'
      instance (Read a, Read b, Read c) => Read (a, b, c)
        -- Defined in `GHC.Read'
      ...plus 25 others
    In the expression: read "1"
    In an equation for `it': it = read "1"
*BTT_02> read "1" + 1
2
*BTT_02> :type it
it :: Integer
*BTT_02> read "1" :: Int
1
*BTT_02> read "1" :: Float
1.0

The Integral Type Class
*BTT_02> (fromIntegral 10) + 10.0
20.0
*BTT_02> (fromIntegral 10) + 10
20
*BTT_02> :type it
it :: Integer
*BTT_02> (fromInteger 10) + 10.0
20.0
*BTT_02> :type it
it :: Double
