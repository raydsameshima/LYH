AF_11.lhs

> module AF_11 where

https://en.wikibooks.org/wiki/Haskell/Category_theory#Functors_on_Hask

Functors in Haskell are from Hask to func, where func is the subcategory of Hask defined on just that functor's types.

class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b

Functor Law 1 (fmap id = id)
The first functor law states that if we map the id function over a functor value, the functor value that we get back should be the same as the original functor value.

Functor Law 2 (fmap (g . f) = fmap g . fmap f) 
The second law says that composing two function over a functor should be the same as first mapping one function over the functor and then mapping other one.

class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

With normal functors, when you map a function over a functor, you can't get the result out in any general way, even if the result is a partially applied functor.
Applicative functors, on the other hand, allow you to operate on several functors with a single function.

Prelude Control.Applicative> Just (+3) <*> Just 9
Just 12
Prelude Control.Applicative> pure (+3) <*> Just 10
Just 13

Applicative Laws
f <*> x                    = fmap f x
pure id <*> v              = v 
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure f <*> pure x          = pure (f x)
u <*> pure y               = pure y ($ y) <*> u
