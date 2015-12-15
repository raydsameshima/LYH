FAFMM_14.lhs

chapter 14
For A Few Monads More

mtl = A monad transfer library

For do-notation desugar:
http://qiita.com/saltheads/items/6025f69ba10267bbe3ee

> module FAFMM_14 where
> import Data.Monoid 
> import Control.Monad.Writer
> import Control.Applicative
> import System.Random
> import Control.Monad.State

import Control.Monad.Instances
  FAFMM_14.lhs:13:3: Warning:
  Module ‘Control.Monad.Instances’ is deprecated:
  This module now contains no instances and will be removed in the future
  Ok, modules loaded: FAFMM_14.

Writer? I Hardly Knew Her!

... we might want to equip our value with string that explain what's going on, probably for debugging purposes.

> isBigGang :: Int -> (Bool, String)
> isBigGang x = (x > 9, "Compared gang size to 9.")

Now what if we already have a value that has a long string attached to it, such as (3, "Smallish gang."), and we want to feed it to isBigGang?

 applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
 applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
 applyLog (x, log) f = let (y, newLog) = f x
                       in  (y, log ++ newLog)

  *> applyLog (3, "this is a test. ") isBigGang 
  (False,"this is a test. Compared gang size to 9.")
  *> (30, "This is a really big number! ") `applyLog` isBigGang 
  (True,"This is a really big number! Compared gang size to 9.")

  *> applyLog ("Tobin", "Got outlaw name.") (\x -> (length x, "applied length."))
  (5,"Got outlaw name.applied length.")
  *> applyLog it isBigGang 
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

Note that, even if we omit type annotation, Haskell will know it, because of append function.

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
2.  do { m1; m2 }        ==> m1 >> do { m2 }
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
Before to do, let's review the functor, applicative and monad:

  *FAFMM_14> :info Functor
  class Functor (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a

  *FAFMM_14> :info Applicative
  class Functor f => Applicative (f :: * -> *) where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    (*>) :: f a -> f b -> f b
    (<*) :: f a -> f b -> f a  

  *FAFMM_14> :info Monad
  class Monad (m :: * -> *) where
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b -- "then"
    return :: a -> m a
    fail :: String -> m a

> gcd'' :: Int -> Int -> Writer [String] Int
> gcd'' a b
>   | b == 0 = tell ["Finished with " ++ show a] 
>              >> (return a) 
>   | otherwise 
>            = tell [show a ++ " mod " ++ 
>                    show b ++ " = " ++ 
>                    show (a `mod` b)
>                   ]
>              >> (gcd'' b (a `mod` b))

As a consequence, it is not so readable.

Inefficient List Construction
When using the Writer monad, we need to be careful which monoid to use, because using lists can sometimes turn out to be very slow.

In our gcd' function, the logging is fast because the list appending ends up looking like this:
  a ++ (b ++ (c ++ (d ++ (e ++ f))))
A list is a data structure that's constructed from left to right.
This is efficient, because we first fully construct the left part of a list and only then add a longer list on the right.
But if we're not careful, using the Writer monad can produce list appending that looks like this:
  ((((a ++ b) ++ c) ++ d) ++ e) ++ f

The following function works like gcd', but in logs stuff in reverse.
First, it produces the log for the rest of the procedure, and then it adds the current step to the end of the log:
  *FAFMM_14> :type tell
  tell :: MonadWriter w m => w -> m ()

> gcdReverse :: Int -> Int -> Writer [String] Int
> gcdReverse a b
>   | b == 0 = do
>       tell["Finished with " ++ show a]
>       return a
>   | otherwise = do
>       result <- gcdReverse b (a `mod` b)
>       tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
>       return result

  *FAFMM_14> runWriter $ gcd'' 8 3
  (1,["8 mod 3 = 2","3 mod 2 = 1","2 mod 1 = 0","Finished with 1"])

  *FAFMM_14> runWriter $ gcdReverse  8 3
  (1,["Finished with 1","2 mod 1 = 0","3 mod 2 = 1","8 mod 3 = 2"])

Let's follow this line by line:
  gcdReverse 8 3
  = do result <- gcdReverse 3 2
       tell ["8 mod 3 = 2"]
       return result
      
  gcdReverse 3 2
  = do result <- gcdReverse 2 1
       tell []
       return result
      
  gcdReverse 2 1
  = do result <- gcdReverse 1 0
                 = do tell ["Finished with 1"]
                      return 1

THIS FUNCTION IS INEFFICIENT BECAUSE IT ENDS UP ASSOCIATING THE USE OF ++ TO THE LEFT INSTEAD OF TO THE RIGHT.
Let's desugar gcdReverse:
do-notation desugar:
(http://qiita.com/saltheads/items/6025f69ba10267bbe3ee)
1.  do { m1 }            ==> m1
2.  do { m1; m2 }        ==> m1 >> do { m2 }
3.  do { let s1; m1 s1 } ==> let s1 in do { m1 s1 }
3'. do { let s1; m1 s1 } ==> do { m1 s1 } where s1
4.  do { x <- m1; m2 x } ==> m1 >>= (\x -> do { m2 x })
5.  do { x <- m1; let s = f x; m2 s } 
                         ==> do { s <- f <$> m1 <*> m2; m3 s }
6.  do { x <- m1; y <- m2; let s = f x y; m3 s } 
                         ==> do { s <- f <$> m1 <*> m2; m3 s }
7. let s1; let s2        ==> let (s1; s2)
8. (\x -> f x)           ==> f

> gcdReverse' :: Int -> Int -> Writer [String] Int
> gcdReverse' a b
>   | b == 0 = tell["Finished with " ++ show a]
>              >> return a
>   | otherwise = 
>       gcdReverse b (a `mod` b)
>       >>= (\result -> tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
>            >> return result)

Finally, I've understand these two different implementations of gcd:
http://qiita.com/bra_cat_ket/items/451dc2977270d686e2ff

Because lists can sometimes be inefficient when repeatedly appaneded in this manner, it's best to use a data structure that always support efficient appending.

Using Difference Lists
(see also http://qiita.com/bra_cat_ket/items/451dc2977270d686e2ff)
While similar to a normal list, a difference list is actually a function that takes a list and prepends another list to it.
E.g. for [1,2,3]
  \xs -> [1,2,3] ++ xs,
and for the normal empty list [],
  \xs -> [] ++ xs
are the corresponding difference lists respectly.

Difference lists support efficient appending.
When we append two normal lists with ++, the code must walk all the way to the end of the list on the left of ++, and then stick the other one there.
Appending two difference lists can be done like so:
  f `append` g = \xs -> f (g xs)
If f = ("dog" ++), g = ("meat" ++), then the right hand side becomes
  \xs -> "dog" ++ ("meat" ++ xs)

Let's make a newtype wrapper for difference lists so that we can easily give them monoid instances:

> newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

Converting are relevant:

> toDiffList :: [a] -> DiffList a
> toDiffList xs = DiffList (xs ++)
> fromDiffList :: DiffList a -> [a]
> fromDiffList (DiffList f) = f []

Here is the Monoid instance

> instance Monoid (DiffList a) where
>   mempty = DiffList (\xs -> [] ++ xs)
>   (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

Note that mempty is just the id, and mappend is just function composition.

Now we can increase the efficiency of our gcdReverse by making it use difference lists:

> fasterGcdReverse :: Int -> Int -> Writer (DiffList String) Int
> fasterGcdReverse a b
>   | b == 0 = do
>       tell (toDiffList ["Finished with " ++ show a])
>       return a
>   | otherwise = do
>       result <- fasterGcdReverse b (a `mod` b)
>       tell (toDiffList [show a ++ " mod " 
>                                ++ show b 
>                                ++ " = " 
>                                ++ show (a `mod` b)
>                        ]
>            )
>       return result

We just needed to change the type of the monoid from [String] to DiffList String and then when using tell, convert our normal lists into difference lists with toDiffList.
  *> mapM_ putStrLn . fromDiffList . snd . runWriter $ fasterGcdReverse 110 34
  Finished with 2
  8 mod 2 = 0
  34 mod 8 = 2
  110 mod 34 = 8

Comparing Performance
To get a feel for just how much difference lists may improve your performance, consider the following function.
It just counts down from some number to zero but produces its log in reverse, like gcdReverse, so that the numbers in the log will actually be counted up.

> finalCountDown :: Int -> Writer (DiffList String) ()
> finalCountDown 0 = do
>   tell (toDiffList ["0"])
> finalCountDown x = do
>   finalCountDown (x-1)
>   tell (toDiffList [show x])

> slowerFinalCountDown :: Int -> Writer [String] ()
> slowerFinalCountDown 0 = do
>   tell ["0"]
> slowerFinalCountDown x = do
>   slowerFinalCountDown (x-1)
>   tell [show x]

By using :set +s, we can estimate the time and memory:
slowerFinalCountDown takes
  5000
  (3.23 secs, 1059636144 bytes)
but finalCountDown, which uses DiffList,  does
  5000
  (0.16 secs, 28076584 bytes)

Of course, this is not the proper and scientific way to test the speed of our programs, however, we were able to see that, in this case, using difference lists starts producing results immediately, whereas normal lists takes forever.

Reader? Ugh, Not This Joke Again
In Chapter 11, we saw that the function type
  (->) r
is an instance of Functor.
  instance Functor ((->) r) where
    fmap = (.)
Mapping a function f over another function g will make a function that the same thing as g, applies g to it, and then applies f to that result.
  Prelude> (fmap (*5) (+3)) 8
  55 
  = (8+3)*5

  *FAFMM_14> let (f,g) = ((*5),(+3))
  *FAFMM_14> fmap f g $ 8
  55
  *FAFMM_14> f . g $ 8
  55

We've seen that functions are applicative functors:

  instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
   
  Prelude Control.Applicative> let f = (+) <$> (*2) <*> (+10)
  Prelude Control.Applicative> f 3
  19
  = ((+) (3+10) (3*2))

Functions As Monads
Not only is the function type (->) r a functor and an applicative functor, but it's also a monad.
Just like other monadic values that you've met so far, a function can also be considered a value with a context.
The context for functions is that value is not present yet and that we need to apply that function to something in order to get its result.
  instance Monad ((->) r) where
    return x = \_ -> x
    h >>= f  = \w -> f (h w) w

The implementations for (>>=) (bind) may seem a bit cryptic, but it's really not all that complicated.
When we use  (>>=) to feed a monadic value to a function, the result is always a monadic value.
So, in this case, when we feed a function to another function, the result is a function as well.
That's why the result starts off as a lmabda.

All of the implementations of (>>=) so far somehow isolated the result from the monadic value and then applied the function f to that result.
The same thing happens here.
To get the result from a function, we need to apply it to something, which is why we use (h w) here, and then we apply f to that.
f returns a monadic value, which is a function in our case, so we aplly it to w as well.

The Reader Monad
If you don't get how (>>=) works at this point, don't worry.

> addStuff :: Int -> Int
> addStuff = do
>   a <- (*2)
>   b <- (+10)
>   return (a+b)
  
  *FAFMM_14 Control.Applicative> addStuff 3
  19

This is the same thing as the applicative expression that we wrote earlier, but now it relies on functions before being monads.
A do expression always results in a monadic value, and this one is no different.
The result of this monadic value is a function.
It takes a number, then (*2) is applied to that number, and the result becomes a.
(+10) is applied to the same number that (*2) was applied to, and the result becomes b.
return, as in other monads, doesn't have any effect but to make a monadic value that presents some results.

Both (*2) and (+10) are applied to the number 3 in this case.
return (a+b) does as as well, but it ignores that value and always presents a+b as the result.
For this reason, the function monad is also called the reader monad.
All the functions read from a common source.
To make this even clearer, we can rewrite addStuff like so:

> addStuff' :: Int -> Int
> addStuff' x = let
>   a = (*2) x
>   b = (+10) x
>   in a+b

You see that the reader monad allows us to treat functions as values with a context.
We can act as if we already know what the functions will return.
It does this by gluing functions together into one function and then giving that function's parameter to all of the functions that compose it.
So, if we have a lot of functions that are all just missing one parameter, and they will eventually be applied to the same thing, we can use the reader monad to sort of extract their future results, and the (>>=) implementation will make sure that it all works out.

Let us try to de-sugar:

> addStuff'' :: Int -> Int
> addStuff'' = (*2) >>= (\a -> ((+10) >>= (\b -> return (a+b))))

or more explicitly,

> addStuff''' =  -- see chapter 13
>   (*2)  >>= (\a ->
>   (+10) >>= (\b ->
>   return (a+b)))

It is much more readable to use Applicative style:

> addStuff'''' = (+) <$> (*2) <*> (+10)
 
Tasteful Stateful Computations
Haskell is a pure language, and because of that, our programs are made of functions that can't change any global state or variables; they can only do some computations and return the results.

However, some problems are inherently stateful, in that they rely on some state that changes over time.
While this isn't a problem for Haskell, these computations can be a bit tedious to model.
That's why Haskell features the State monad, which makes dealing with stateful problems a breeze, while still keeping everything nice and pure.

When we were looking at random numbers back in Chapter 9, we dealt with functions that took a random generator as a parameter and returned a random number and a new random generator.
If we wanted to generate several random numbers, we always needed to use the random generator that a previous function returned along with its result.
For example, to create a function that takes a StdGen and tosses a coin three times based on that generator, we did this:

> threeCoins :: StdGen -> (Bool, Bool, Bool)
> threeCoins gen =
>   let (firstCoin, newGen)   = random gen
>       (secondCoin, newGen') = random newGen
>       (thirdCoin, newGen'') = random newGen'
>   in  (firstCoin, secondCoin, thirdCoin)

  *FAFMM_14> threeCoins (mkStdGen 21)
  (True,True,True)

This function takes a generator gen, and then random gen returns a Bool value along with a new generator.
To throw the second coin, we use the new generator, and so on.

In most other languages, we wouldn't need to return a new generator along with a random number.
We could just modify the existing one!
But since Haskell is pure, we can't modify the existing one, so we need to take some state, make a result from it and a new state, and then use that new state to generate new results.

You would think that to avoid manually dealing with stateful computations in this way, we would need to give up the purity of Haskell.
Well, we don't have to, since there's a special little monad called State monad that handles all this state business for us, without impacting any of the purity that makes Haskell programming so cool.

Stateful Computations
To help demonstrate stateful computations, let's go ahead and give them a type.
We'll say that a stateful computation is a function that takes some state and returns a value along with some new state.
That function has the follwing type:
  s -> (a, s)
where s is the type of the state, and a is the result of the stateful computations.

A function that takes a state and returns a result and a new state.
This can be thought of as a value with a context as well.

Stacks and Stones
  LIFO: Last In First Out; FILO: First In Last Out

A stack is a data structure that contains a bunch of elements and supports exactly two operations:
  -Pushing an element to the stack, which adds an element on the top.
  -Popping an element off the stack, which removes from the top.

We'll use a list to represent our stack, with the head of the list acting as the fop of the stack.
To help us with our task, we'll make 2 functions:

> type Stack = [Int]
>
> pop' :: Stack -> (Int, Stack)
> pop' (x:xs) = (x,xs)
>
> push' :: Int -> Stack -> ((), Stack)
> push' a xs = ((), a:xs)
 
  Prelude> :type () -- unit
  () :: ()
We used () as the result when pushing to the stack because pushing an item onto the stack doesn't have any important result value - its main job is to change the stack.
Let's write a small piece of code to simulate a stack using these functions.
We'll take a stack, push 3 to it, and then pop two items, just for kicks.

> stackManip' :: Stack -> (Int, Stack) -- page 315
> stackManip' stack = 
>   let ((), newStack1) = push' 3 stack
>       (_ , newStack2) = pop' newStack1
> --    (a , newStack2) = pop' newStack1
>   in  pop' newStack2

Well, using the State monad will allow us to do exactly that.

> stackManip'' :: Stack -> (Int, Stack)
> stackManip'' = do
>   push' 3
>   _ <- pop'
> -- a <- pop'
>   pop'

or desugared version

> stackManip''' :: Stack -> (Int, Stack)
> stackManip''' = push' 3 >> pop' >> pop'

With it, we will be able to take stateful computations like these and use them without needing to manage the state manually.
  
The State Monad
The Control.Monad.State provides a newtype that wraps stateful computations.

  newtype State s a = State {runState :: s -> (a,s)}

A State s a is a stateful computation that manipulates a state of type s and has a result of type a.

Let's check out their Monad instance:

  instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in  g newState

Our aim with return is to take a value and make a stateful computation that always has that value as its result.
We always present x as the result of the stateful computation, and the state is kept unchanged, because return must put a value in a minimal context, see the wrapped type in State.
So return will make a stateful computation that presents a certain value as the result and keeps the state unchanged.

It's easy to wrap pop and push into a State wrapper:

> pop :: State Stack Int
> pop = state $ \(x:xs) -> (x,xs)
> push :: Int -> State Stack ()
> push a = state $ \xs -> ((), a:xs)

> stackManip :: State Stack Int
> stackManip = do
>   push 3
>   pop
> -- _ <- pop -- We didn't need to bind this pop to anything.
>   pop
>
> stackManipDesugared = push 3 >> pop >> pop 
  
  *FAFMM_14> runState stackManipDesugared  [5,8,2,1]
  (5,[8,2,1])
  *FAFMM_14> runState stackManip  [5,8,2,1]
  (5,[8,2,1])

... if we want to do something a little more complicated?

> stackStuff :: State Stack ()
> stackStuff = do
>   a <- pop
>   if a == 5
>     then push 5
>     else do push 3
>             push 8
  
  *FAFMM_14> runState stackStuff [4]
  ((),[8,3])
  *FAFMM_14> runState stackStuff [5]
  ((),[5])
  *FAFMM_14> runState stackStuff [1..4]
  ((),[8,3,2,3,4])

> moreStack :: State Stack ()
> moreStack = do
>   a <- stackManip
>   if a == 100
>     then stackStuff
>     else return ()

Getting and Setting State

