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
