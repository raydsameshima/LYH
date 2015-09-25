AFOM_13.lhs

A FISTFUL OF MONADS

> module AFOM_13 where

In this chapter, you'll learn about monads, which are just beefed-up applicative functors, much like applicative functors are beefed-up functors.

Upgrading Our Applicative Functors
...Monads are a natural extension of applicative functors, and they provide a solution to the following problem: if we have a value with a context,
  m a,
how do we apply to it a function that takes a normal a and returns a value with a context?
In other words, how do we apply a function of a type
  a -> m b
to a value of type m a?
Essentially, we want this function:
  (>>=) :: (Monad m) => ma -> (a -> m b) -> m b

If we have a fancy value (i.e. a value with contexts) and a function that takes a normal value but returns a fancy value, how do we feed that fancy value into the function?
This is the main concern when dealing with monads.
We write
  m a
instead of
  f a,
because the m stands for Monad, but monads are just applicative functors that support (>>=).
The (>>=) function is called bind.

Getting Your Feet Wet with Maybe
... Maybe is a monad.

... let's think about how we would use (>>=) with Maybe.
(>>=) takes a monadic value and a function that takes a normal value.
It returns a monadic value and manages to apply that function to the monadic value.
How does it do that if the function takes a normal value?
Well, it must take into account the context of that monadic value.

In this case, (>>=) would take a Maybe a value and a function of type a -> Maybe b, and somehow apply the function to the Maybe a.

> applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
> applyMaybe Nothing f = Nothing
> applyMaybe (Just x) f = f x

The Monad Type Class
  class Monad m where 
  -- class (Applicative m) => Monad m where
    return :: a -> ma -- same as pure from Applicative

    (>>=) :: m a -> (a -> m b) -> m b -- "bind"

    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail msg = error msg

return just takes a normal value and puts it in a context.

bind (>>=) is like function application, but instead of taking a normal value and feeding it to a normal function, it takes a monadic value (that is, a value with a context) and feeds it to a function that takes a normal value but returns a monadic value.

Next up, we have >>.
We won't pay too much attention to it for now because it comes with a default implementation, and it's rarely implemented when making Monad instances.

Now that you know what the Monad type class looks like, let's take a look at how Maybe is an instance of Monad:
  instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >> f = f x -- same as our applyMaybe
    fail _ = Nothing

Example
  *AFOM_13> return "What" :: Maybe String
  Just "What"
  *AFOM_13> :type return
  return :: Monad m => a -> m a
  *AFOM_13> Just 9  >>= \x -> return (x*10)
  Just 90
  *AFOM_13> Nothing >>= \x -> return (x*19)
  Nothing
 
It seems as though we were able to extract the value from a Maybe without pattern matching.   
And we still didn't lose the context of our Maybe value, because it's Nothing, the result (>>=) will be Nothing as well.

Walk the Line
Now that you know how to feed a (Maybe a) value to a function of type
  a -> Maybe b
while taking into account the context of possible failure, let's see how we can use (>>=) repeatedly to handle computations of several (Maybe a) value.

Code, Code, Code.
We can represent the pole with a simple pair of integers.

> type Birds = Int
> type Pole = (Birds, Birds)

> landLeft, landRight :: Birds -> Pole -> Maybe Pole
> landLeft  n (l, r) 
>   | abs (l+n-r) < 4 = Just (l+n, r)
>   | otherwise       = Nothing
>
> landRight n (l, r) 
>   | abs (l-r-n) < 4 = Just (l, r+n)
>   | otherwise       = Nothing

  *AFOM_13> landLeft 2 (0,0)
  Just (2,0)
  *AFOM_13> it >>= (landRight 3)
  Just (2,3)
  *AFOM_13> return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
  Just (2,4)

Banana on a Wire

> banana :: Pole -> Maybe Pole
> banana _ = Nothing

  *AFOM_13> return (0,0) >>= landLeft 1 >>= landRight 1 >>= banana >>= landRight 2 
  Nothing

Instead of making functions that ignore their input and just return a predetermined monadic value, we can use the (>>) function:
  *AFOM_13> return (0,0) >>= landLeft 1 >>= landRight 1 >> Nothing >>= landLeft (-1)
  Nothing
  *AFOM_13> return (0,0) >>= landLeft 2
  Just (2,0)
  *AFOM_13> it >> return (1,1) >>= landRight 2
  Just (1,3)

What would this look like if we hadn't made the clever choice of treating Maybe value as values with a failure context and feeding them to function?

  routine :: Maybe Pole
  routine = case landLeft 1 (0,0) of
    Nothing    -> Nothing
    Just pole1 -> case landRight 4 pole1 of
      Nothing    -> Nothing
      Just pole2 -> case landLeft 2 pole2 of
        Nothing    -> Nothing
        Just pole3 -> landLeft 1 pole3
