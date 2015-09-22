Monoids_12.lhs

> module Monoids_12 where

> import Data.Monoid
> import qualified Data.Foldable as F
> import MOOTATC_07 

Wrapping an Existing Type into a New Type (newtype keyword)

tensor like behavior
  > [(+1), (*100), (*5)] <*> [1,2,3]
  [2,3,4,100,200,300,5,10,15]
  > :type (<*>)
  (<*>) :: Applicative f => f (a -> b) -> f a -> f b

direct product like behavior
  > ZipList [(+1), (*100), (*5)] <*> ZipList [1,2,3]
  ZipList {getZipList = [2,200,15]}
  > :info ZipList
    newtype ZipList a = ZipList {getZipList :: [a]}
      -- Defined in ‘Control.Applicative’
    instance Eq a => Eq (ZipList a) -- Defined in ‘Control.Applicative’
    instance Functor ZipList -- Defined in ‘Control.Applicative’
    instance Ord a => Ord (ZipList a)
      -- Defined in ‘Control.Applicative’
    instance Read a => Read (ZipList a)
      -- Defined in ‘Control.Applicative’
    instance Show a => Show (ZipList a)
      -- Defined in ‘Control.Applicative’
    instance Applicative ZipList -- Defined in ‘Control.Applicative’

The newtype keyword in Haskell is made exactly for cases when we want to just take one type and wrap it in something to present it as another type.
(we can also use data keyword, but for wrapping, newtype keyword is faster.)
If you want it to be the same internally but have a different type, use newtype keyword.

When you make a new type from an existing type by using the newtype keyword, you can have only one value constructor, and that value constructor can have only one filed.
But with data keyword, you can make data types that have several value constructors, and each constructor can have zero or more fields:
  data Profession = Fighter | Archer | Accountant
  data Race = Human | Elf | Orc | Goblin
  data PlayerCharacter = PlayerCharacter Race Profession

Example:

> newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

  CharList {getCharList = "this will be shown!"}
  *Monoids_12> CharList "bunny" == CharList "benny"
  False
  *Monoids_12> CharList "bunny" == CharList "bunny"
  True
  *Monoids_12> getCharList C
  Char      CharList  CharList  Circle
  *Monoids_12> getCharList CharList "aiueo"
  *Monoids_12> getCharList $ CharList "aiueo"
  "aiueo"

Using newtype to Make Type Class Instances

> newtype Pair b a = Pair { getPair :: (a,b)} 
>   deriving (Show)
> instance Functor (Pair c) where
>   fmap f (Pair (x,y)) = Pair (f x, y)

  *Monoids_12> fmap (*100) (Pair (2,3))
  Pair {getPair = (200,3)}
  *Monoids_12> getPair $ fmap (*100) (Pair (2,3))
  (200,3)
  *Monoids_12> fmap (+10) it
  Pair {getPair = (210,3)}
  *Monoids_12> fmap reverse $ Pair ("London calling", 3)
  Pair {getPair = ("gnillac nodnoL",3)}

On newtype Laziness
The only thing that can be done with newtipe keyword is turning an existing type into a new type, so internally, Haskell can represent the values of types defined with newtype just like the original ones, while knowing that their types are now distinct.
This means that not only is newtype usually faster than data, its pattern-matching mechanism is lazier.

Haskell is lazy by default, so called the "call-by-need" evaluation strategy.
  Prelude> :type undefined
  undefined :: t
  Prelude> head [3,4,5,undefined]
  3
  Prelude> [3,3,3,undefined]
  [3,3,3,*** Exception: Prelude.undefined

> data CoolBool = CoolBool { getCoolBool :: Bool }
> helloMe :: CoolBool -> String
> helloMe (CoolBool _) = "hello"

  *Monoids_12> helloMe (CoolBool True)
  "hello"
  *Monoids_12> helloMe (CoolBool undefined)
  "hello"
  *Monoids_12> helloMe undefined
  "*** Exception: Prelude.undefined

Why did this exception happen?
Types defined with the data keyword can have multiple value constructors (even though CoolBool has only one).
So in order to see if the value given to our function conforms to the 
  (CoolBool _) 
pattern, Haskell must evaluate the value just enough to see which value constructor was used when we made the value.
And when we try to evaluate an undefined value, even a little, an exception is thrown.

Instead of using the data keyword for CoolBool, let's try using newtype:

> newtype CoolerBool = CoolerBool { getCoolerBool :: Bool }
> pleaseHelloMe :: CoolerBool -> String
> pleaseHelloMe (CoolerBool _) = "hello!"

  *Monoids_12> helloMe undefined 
  "*** Exception: Prelude.undefined
  *Monoids_12> pleaseHelloMe undefined
  "hello!"

It works, but why?
Using newtype keyword, Haskell can internally represent the value of the new type in the same way as the original values.
It doesn't need to add another box around them; it just must be aware of the values being of different types.
And because Haskell knows that types made with the newtype keyword can have only one constructor, it doesn't need to evaluate the value passed to the function to make sure that the value conforms to the
  (CoolerBool _)
pattern, because newtype types can have only one possible value constructor and one field!

type vs. newtype vs. data

type for type synonyms, if you want your type signatures to look cleaner and be more descriptive.
If you want to an existing type and wrap it in a new type in order to make it an instance of a type class, chances are you're looking for a newtype.
If you want to make something completely new, odds are good that you're looking for the data keyword.

About Those Monoids
A monoid is a structure set M with associative binary operation *:M -> M -> M with unit:
  c*(b*a) = cba = (c*b)*a
  e*m = m = m*e

The Monoid Type Class
A monoid is made up of an associative binary funcion and a value that acts as an identity with respect to that function.

  class Monoid m where
    mempty :: m            -- identity
    mappend :: m -> m -> m -- binary operation
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty -- guaranteed by the associativity

The Monoid Laws
The identity law and the associativity.

Meet Some Monoids
Lists Are Monoids
Product and Sum

> newtype MyProduct a = MyProduct { getMyProduct :: a }
>   deriving (Eq, Ord, Read, Show, Bounded)
> instance Num a => Monoid (MyProduct a) where
>   mempty = MyProduct 1
>   MyProduct x `mappend` MyProduct y = MyProduct (x*y)
  
  *Monoids_12> MyProduct 3 `mappend` MyProduct 9
  MyProduct {getMyProduct = 27}
  *Monoids_12> MyProduct 3 `mappend` mempty 
  MyProduct {getMyProduct = 3}
  *Monoids_12> mempty `mappend` MyProduct 7
  MyProduct {getMyProduct = 7}
  *Monoids_12> mconcat . map MyProduct $ [3,4,5]
  MyProduct {getMyProduct = 60}
  *Monoids_12> getMyProduct it
  60

Any and All

The Ordering Monoid
  instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT
The instance is set up like this:
When we mappend two Ordering values, the one on the left is kept, unless the value on the left is EQ, the right one is the result.

...but it actually resembles the way we alphabetically compare words.
We look at the first two letters, and if they differ, we can already decide which word would go first in a dictionary.
However, if the first two letters are equal, then we move on to comparing the next pair of letters and repeat the process.

  *Monoids_12> ('a' `compare` 'a') `mappend` ('t' `compare` 's') 
  GT
  *Monoids_12> "at" `compare` "as"
  GT

Let's say we are writing a function that takes two strings, compares their lengths, and returns an Ordering.
But if the strings are of the same length, instead or retuning EQ right away, we want to compare them alphabetically:

> lengthCompare :: String -> String -> Ordering
> lengthCompare x y 
>   = (length x `compare` length y) `mappend` (x `compare` y)

  *Monoids_12> lengthCompare "zen" "ants"
  LT
  *Monoids_12> lengthCompare "zen" "ant"
  GT

Remember that when we use mappend, its left parameter is kept unless it's EQ; if it's EQ, the right one is kept.
That's why we put the comparison that we consider to be the first, more important, criterion as the first parameter.
Now suppose that we want to expand this function to also compare for the number of vowels and set this to be second most important criterion for comparison:
  lengthCompare x y 
    = (length x `compare` length y) `mappend`
      (vowels x `compare` vowels y) `mappend`
      (x `compare` y)
      where vowels = length . filter (`elem` "aiueo`)

Maybe the Monoid
Let's take a look at the various ways that Maybe a can be made an instance of Monoid and how those instances are useful.

wrapping
  instance Monoid m => Monoid (Maybe m) where
    mempty = Nothing  -- identity
    Nothing `mappend` a = a
    a `mappend` Nothing = a
    Just a1 `mappend` Just a2 = Just (a1 `mappend` a2)

  *Monoids_12> Just "Judy and " `mappend` Just "Mary"
  Just "Judy and Mary"

But what if the type of the contents of the Maybe is not an instance of Monoid?
...one thing we can do is discard the second value and keep the first one.
For this purpose, the 
  First a
exists:
  newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)
  instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x

First is useful when we have a bunch of Maybe values and just want to know if any of them is a Just.
  *Monoids_12> mconcat . map First $ [Nothing, Just 9, Just 10]
  First {getFirst = Just 9}

If we want a monoid on Maybe a such that the second parameter is kept if both parameters of mappend are Just values, Data.Monoid provides the Last a type, which works like First a, but the last non-vanishing value is kept when mappending and using mconcat:
  *Monoids_12> mconcat . map Last $ [Nothing, Just 9, Just 10, Nothing]
  Last {getLast = Just 10}

Folding with Monoids
...there are so many data structures that work nicely with folds, the Foldable type class was introduced.

One way to make a type constructor an instance of Foldable is to just directly implement foldr for it.
But another, often much easier way, is to implement the foldMap function, which is also a part of the Foldable type class.
The foldMap function has the following type:
  foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
Its first parameter is a function that takes a value of the type that our foldable structure contains (denoted here with a) and returns a monoid value.
Its second parameter is a foldable structure that contains values of type a.
It maps that function over the foldable structure, thus producing a foldable structure that contains monoid values.
Then, by doing mappend between those monoid values, it joints them all into a single monoid value.
This function may sound kind of odd at the moment, but you'll see that it's our type to be made an instance of Foldable!
So if we just implement foldMap for some type, we get foldr and foldl on that type for free.

This is how we make Tree an instance of Foldable:

> instance F.Foldable Tree where
>   foldMap f EmptyTree = mempty
>   foldMap f (Node x left right) = F.foldMap f left `mappend`
>                                   f x              `mappend`
>                                   F.FoldMap f right

  instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x left right) =
      f x `mappend` F.foldMap f left `mappend` F.foldMap f right

> testTree = Node 5
>              (Node 3
>                 (Node 1 EmptyTree EmptyTree)
>                 (Node 6 EmptyTree EmptyTree)
>              )
>              (Node 9
>                 (Node 8 EmptyTree EmptyTree)
>                 (Node 10 EmptyTree EmptyTree)
>              )

We can also easily turn our tree into a list by doing a foldMap with
  \x -> [x]
function.
By first projecting that function onto our tree, each element becomes a singleton list.
The mappend action that takes place between all those singleton lists results in a single list that holds all of the elements that are in our tree:
  *Monoids_12> F.foldMap (\x -> [x]) testTree
  [5,3,1,6,9,8,10]
  *Monoids_12> testTree 
  Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 6 EmptyTree EmptyTree)) (Node 9 (Node 8 EmptyTree EmptyTree) (Node 10 EmptyTree EmptyTree))

