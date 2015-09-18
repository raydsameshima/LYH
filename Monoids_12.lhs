Monoids_12.lhs

> module Monoids_12 where

> import Data.Monoid
> import qualified Data.Foldable as F
> import MOOTATC_07 

Wrapping an Existing Type into a New Type

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

