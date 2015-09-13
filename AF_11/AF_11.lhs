AF_11.lhs

> module AF_11 where

... and type classes makes implementing polymorphism much easier than in other languages.

https://en.wikibooks.org/wiki/Haskell/Category_theory#Functors_on_Hask
Functors in Haskell are from Hask to func, where func is the subcategory of Hask defined on just that functor's types.

class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  
"Give me a function of (a -> b) and a box with an a (or several of them) inside it, and I'll give with a b (or several of them) inside it."

We can also look at functor values as values with an added context.
  Maybe :: they might have failed
  lists :: the values can actually be several values at once or none
fmap applies a function to the value while preserving its context.

I/O Actions As Functors

instance Functor IO where
  fmap f action = do
    result <- action
    return (f result)
    
The result of mapping something over an I/O action will be an I/O action, so right off the bat(at the very beginning), we use the do syntax to glue two actions and make a new one.
In the implementation for fmap, we make a new I/O action that first performs the original I/O action and calls its result result.
Then we do return (f result).
Recall that return is a function that makes an I/O action that doesn't do anything but only yields something as its result.
  return :: Monad m => a -> m a

Example:
  main = do
    line <- fmap reverse getLine
    -- line' <- getLine
    -- let line = reverse line'
    putStrLn $ "You said " ++ line ++ " backwards!"
    putStrLn $ "Yes, you really said " ++ line ++ " backwards!"

  fmap :: Functor f => (a -> b) -> f a -> f b
  reverse :: [] a -> [] a
  getLine :: IO String

By definition, 
  fmap reverse getLine = do result <- getLine
                            return (reverse result)
Just as we can (fmap reverse) over Just "Blah" to get Just "halb",
  Prelude> fmap reverse $ Just "Blah"
  Just "halB"
we can fmap reverse over getLine.

Functions As Functors

  instance Functor ((->) r) where
    fmap f g (\x -> f (g x))
The (co)restriction of fmap is
  fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
or
  fmap :: (a -> b) -> (r -> a) -> (r -> b)
i.e. fmap is just the function composition.
Therefore, we have another way to write this instane:
  instance Functor ((->) r) where
    fmap = (.)

Example:
  Prelude> :type fmap (*3) (+100)
  fmap (*3) (+100) :: Num a => a -> a
  Prelude> fmap (*3) (+100) 1
  303
  Prelude> (*3) `fmap` (+100) $ 1
  303

We can call fmap as an infix function so that the resemblance to . is clear.
Just like all functors, functions can be thought of as values with contexts.
When we have a function like (+3), we can view the value as the eventual result of the function (since all function is not an instance of Show), and the context is that we need to apply the function to something to get to the result.

If we write
  fmap :: (a -> b) -> f a -> f b
as
  fmap :: (a -> b) -> (f a -> f b)
then, we can think of fmap (not as a function that takes one function and a functor value and returns a functor value,) but as a function that takes a function and returns a new function that’s just like the old one, except that it takes a functor value as a parameter and returns a functor value as the result.
It takes a function (a -> b) and returns a function (f a -> f b).
This is called lifting a function, e.g.
  Prelude> :type fmap (*2)
  fmap (*2) :: (Num b, Functor f) => f b -> f b
  Prelude> :t fmap (replicate 3)
  fmap (replicate 3) :: Functor f => f a -> f [a]

  Prelude> fmap (*2) $ Just 3
  Just 6
  Prelude> fmap (replicate 3) $ Just 10
  Just [10,10,10]
  Prelude> fmap (replicate 3) $ "Haskell"
  ["HHH","aaa","sss","kkk","eee","lll","lll"]

Functor Law 1: fmap id = id

The first functor law states that if we map the id function over a functor value, the functor value that we get back should be the same as the original functor value.
  id :: a -> a
  fmap :: Functor f => (a -> b) -> f a -> f b
  (fmap id) :: Functor f => f b -> f b
Example:
  fmap id (Just 3) = Just 3 = id (Just 3)
  fmap id [1..5] = [1,2,3,4,5] = id [1..5]

Functor Law 2: fmap (g . f) = fmap g . fmap f 

The second law says that composing two function over a functor should be the same as first mapping one function over the functor and then mapping other one.
  fmap :: Functor f => (a -> b) -> f a -> f b

Using Applicative Functors

  class Functor f => Applicative (f :: * -> *) where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

pure should take a value of any type and return an applicative value with that value inside it.
We take a value and wrap it in an applicative value ("default" value) that has that value as the result inside it.
  Prelude> pure "Hey" :: Maybe String
  Just "Hey"
  Prelude> pure "Hey" :: [String]
  ["Hey"]
  Prelude> pure "Hey" :: [] String
  ["Hey"]

(<*>) can be seen as a beefed-up fmap:
  fmap :: (a -> b) -> f a -> f b
Whereas fmap takes a function and a functor value that has a function inside the functor value, (<*>) takes a functor value that has a function in it and another functor, and extracts that function from first functor and then maps it over the second one.

With normal functors, when you (f)map a function over a functor, you can't get the result out in any general way, even if the result is a partially applied functor.
Applicative functors, on the other hand, allow you to operate on several functors with a single function.

Maybe the Applicative Functor

  instance Applicative Maybe where
    pure = Just
    Nothing  <*> _         = Nothing
    (Just f) <*> something = fmap f something

The Applicative Style

  Prelude Control.Applicative> pure (+) <*> ([3]) <*> ([5])
  [8]
  Prelude Control.Applicative> pure (+) <*> Just 3 <*> Just 5
  Just 8

  Prelude Control.Applicative> Just (+3) <*> Just 9
  Just 12
  Prelude> Nothing <*> Just "ppp"
  Nothing

The last line is interesting, because we try to extract a function from a Nothing and then map it over something, which results in Nothing.

  (<$>) :: (Functor f) => (a -> b) -> f a -> f b
  f <$> x = fmap f x

By using <$>, the applicative style really, shines, because now if we want to apply a function f between three applicative values, we can write
  f <$> x <*> y <*> z
instead of
  fmap f x <*> y <*> z = pure f <*> x <*> y <*> z   

Example:
  Prelude> pure (++) <*>  Just "johntra" <*> Just "volta"
  Just "johntravolta"
  Prelude> fmap (++) (Just "johntra") <*> Just "volta"
  Just "johntravolta"
  Prelude> (++) <$> Just "johntra" <*> Just "volta"
  Just "johntravolta"
  Prelude> (++) "johntra" "volta"
  "johntravolta"

Lists
  instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

The restriction of <*> on lists, we would have
  (<*>) :: [] (a -> b) -> [] a -> [] b 
or more simply
  (<*>) :: [a -> b] -> [a] -> [b]

Using the applicative style on lists is often a good replacement for list comprehensions.
  Prelude> [ x*y | x <- [2,5,10], y <- [8,10,11]]
  [16,20,22,40,50,55,80,100,110]
  Prelude> (*) <$> [2,5,10] <*> [8,10,11]
  [16,20,22,40,50,55,80,100,110]
... it's easier to see that we're just calling * between two "nondeterministic" computations:
  Prelude> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
  [55,80,100,110]

IO Is An Applicative Functor, Too
  instance Applicative IO where
    pure = return
    a <*> b = do f <- a
                 x <- b
                 return (f x)
see 
http://d.hatena.ne.jp/kazu-yamamoto/20101211/1292021817

Applicative Laws
  f <*> x                    = fmap f x
  pure id <*> v              = v 
  pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
  pure f <*> pure x          = pure (f x)
  u <*> pure y               = pure y ($ y) <*> u
