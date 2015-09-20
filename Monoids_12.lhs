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

> instance F.Foldable Tree where
>   foldMap f EmptyTree = mempty
>   foldMap f (Node x left right) =
>     f x `mappend` F.foldMap f left `mappend` F.foldMap f right
>
> testTree = Node 5
>              (Node 3
>                 (Node 1 EmptyTree EmptyTree)
>                 (Node 6 EmptyTree EmptyTree)
>              )
>              (Node 9
>                 (Node 8 EmptyTree EmptyTree)
>                 (Node 10 EmptyTree EmptyTree)
>              )

