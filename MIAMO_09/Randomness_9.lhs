Randmness_9.lhs
from Chapter 9

> module Randmness_9 where

to use the following functions in GHCi, use :load

> import System.Random

> threeCoins :: StdGen -> (Bool, Bool, Bool)
> threeCoins gen =
>   let (firstCoin, newGen)   = random gen
>       (secondCoin, newGen') = random newGen
>       (thirdCoin, newGen'') = random newGen'
>   in (firstCoin, secondCoin, thirdCoin)

using where

> threeCoins' :: StdGen -> (Bool, Bool, Bool)
> threeCoins' gen = (fC,sC,tC)
>   where (fC, newGen1) = random gen
>         (sC, newGen1') = random newGen1
>         (tC, newGen1'') = random newGen1'

> finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => -- Eq n を足せと怒られた
>                  n -> g ->([a],g)
> finiteRandoms 0 gen = ([], gen)
> finiteRandoms n gen =
>   let (value, newGen) = random gen
>       (restOfList, finalGen) = finiteRandoms (n-1) newGen
>   in (value:restOfList, finalGen)
