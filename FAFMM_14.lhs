FAFMM_14.lhs

chapter 14
For A Few Monads More

For do-notation desugar:
http://qiita.com/saltheads/items/6025f69ba10267bbe3ee


> module FAFMM_14 where
> import Data.Monoid 
> import Control.Monad.Writer
> import Control.Applicative

Writer? I Hardly Knew Her!

... we might want to equip our value with string that explain what's going on, probably for debugging purposes.

> isBigGang :: Int -> (Bool, String)
> isBigGang x = (x > 9, "Compared gang size to 9.")

Now what if we already have a value that has a long string attached to it, such as (3, "Smallish gang."), and we want to feed it to isBigGang?

 applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
 applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
 applyLog (x, log) f = let (y, newLog) = f x
                       in  (y, log ++ newLog)

  > applyLog (3, "this is a test. ") isBigGang 
  (False,"this is a test. Compared gang size to 9.")

  > applyLog ("Tobin", "Got outlaw name.") (\x -> (length x, "applied length."))
  (5,"Got outlaw name.applied length.")
  > applyLog it isBigGang 
  (False,"Got outlaw name.applied length.Compared gang size to 9.")

Monoids to Rescue

Right now, applyLog takes values of type (a, String), but is there a reason that the log must be a String?
It uses (++) to append the logs, so wouldn't this work on any kind of list, not just a list of characters?

  applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])

Now the log is a list.
Would this work for bytesrings?
There's no reason it shouldn't.
However, the type we have now works only for lists.

Both lists and bytestrings are monoids, i.e. both are instances of the Monoid type class, which means that they implement the (mappend) function.

Now our applyLog can work for any monoid, any Monoid type class instance:

> applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
> applyLog (x, log) f = let (y, newLog) = f x
>                       in  (y, log `mappend` newLog)

Or using where,

> -- applyLog' :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
> applyLog' (x,log) f = (y, newerLog)
>   where (y, newLog) = f x
>         newerLog = log `mappend` newLog

Note that, even if we omit type annotation, Haskell will know it.

Because the accompanying value can now be any monoid value, we no longer need to think of the tuple as a value and a log; now we can think of it as a value with an accompanying monoid value.

> type Food = String
> type Price = Sum Int
>
> addDrink :: Food -> (Food, Price)
> addDrink "beans" = ("milk", Sum 25)
> addDrink "jerky" = ("whisky", Sum 99)
> addDrink _       = ("beer", Sum 30)

The Writer Type
To attach a monoid to a value, we just need to put them together in a tuple:
  newtype Writer w a = Writer { runWriter :: (a,w)}
Its Monad instance is defined like so:
  instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x,mempty)
    (Writer (x,v)) >>= f = let (Writer (y,v')) = f x
                           in  Writer (y,v `mappend` v')
(>>=)'s implementation is essentially the same as applyLog, only now that tuple is wrapped in the Writer newtype.

  Control.Monad.Writer> runWriter (return 3 :: Writer [a] Int)
  (3,[])
  Control.Monad.Writer> runWriter (return 3 :: Writer String Int)
  (3,"")
  Control.Monad.Writer> runWriter (return 3 :: Writer (Sum Int) Int)
  (3,Sum {getSum = 0})
  Control.Monad.Writer> runWriter (return 3 :: Writer (Product Int) Int)
  (3,Product {getProduct = 1})

Using do Notation with Writer
Now that we have a Monad instance, we're free to use do notation for Writer values.

> logNumber :: Int -> Writer [String] Int
> logNumber x = writer (x, ["Got number: " ++ show x])
>
> multWithLog, multWithLog':: Writer [String] Int
> multWithLog = do
>   a <- logNumber 3
>   b <- logNumber 5
>   return (a*b)
>
> multWithLog' = (*) <$> (logNumber 3) <*> (logNumber 5)

ogNumber takes a number and makes a Writer value out of it.
Notice how we used the writer function to construct a Writer value, instea of directly using the Writer value constructor.
For a monoid, we use a list of strings, and we equip the number with a singleton list that just says that we have that number.
  
  *FAFMM_14> runWriter multWithLog
  (15,["Got number: 3","Got number: 5"])
  *FAFMM_14> runWriter multWithLog'
  (15,["Got number: 3","Got number: 5"])

Someimes, we just want some monoid value to be included at some particular point.
For this, the tell function is useful.
It's part of the MonadWriter type class.
In the case of Writer, it takes a monoid value, like
  ["this ios going on"],
and creates a Writer value hat presents the dummy value () as its result, but has the desired monoid value attached.
When we have a monadic value that has () as its result, we don't bind it to a variable.

> newMultWithLog :: Writer [String] Int
> newMultWithLog = do
>   a <- logNumber 3
>   b <- logNumber 5
>   tell ["Gonna multiply these two"]
>   return (a*b)

Adding Loggin to Programs
Euclid's algorithm takes two numbers and computes their greatest common divisor, that is, the biggest number that still divides both of them.

> gcd' :: Int -> Int -> Writer [String] Int
> gcd' a b
>   | b == 0 = do
>       tell ["Finished with " ++ show a] 
>       return a
>   | otherwise = do
>       tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
>       gcd' b (a `mod` b)

Instead of this do expression, we could have also written this
  writer (a, ["Finished with " ++ show a]) 

  *FAFMM_14> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
  8 mod 3 = 2
  3 mod 2 = 1
  2 mod 1 = 0
  Finished with 1
  *FAFMM_14> runWriter (gcd' 8 3)
  (1,["8 mod 3 = 2","3 mod 2 = 1","2 mod 1 = 0","Finished with 1"])

do-notation desugar:
(http://qiita.com/saltheads/items/6025f69ba10267bbe3ee)
1.  do { m1 }            ==> m1
2.  do { m1, m2 }        ==> m1 >> do { m1 }
3.  do { let s1; m1 s1 } ==> let s1 in do { m1 s1 }
3'. do { let s1; m1 s1 } ==> do { m1 s1 } where s1
4.  do { x <- m1; m2 x } ==> m1 >>= (\x -> do { m2 x })
5.  do { x <- m1; let s = f x; m2 s } 
                         ==> do { s <- f <$> m1 <*> m2; m3 s }
6.  do { x <- m1; y <- m2; let s = f x y; m3 s } 
                         ==> do { s <- f <$> m1 <*> m2; m3 s }
7. let s1; let s2        ==> let (s1; s2)
8. (\x -> f x)           ==> f

Let us rewrite above do-notation:

> gcd'' :: Int -> Int -> Writer [String] Int
> gcd'' a b
>   | b == 0 = do
>       tell ["Finished with " ++ show a]
>       return a
>   | otherwise = do
>       tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
>       gcd'' b (a `mod` b)


