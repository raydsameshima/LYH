ZIP_15.lhs

Chapter 15 ZIPPER

> module ZIP_15 where

In this chapter, you'll see how to take some data structure and equip it with something called a zipper to focus on a part of the data structure in a way that makes changing its elements easy and walking around it efficient.

Taking a Walk
Let's pick a seed that we'll use to plant ours:

> data Tree a = Empty
>             | Node a (Tree a) (Tree a)
>             deriving (Show, Eq)
>
> freeTree :: Tree Char
> freeTree =
>   Node 'P'
>     (Node 'O'
>       (Node 'L'
>         (Node 'N' Empty Empty)
>         (Node 'T' Empty Empty)
>       )
>       (Node 'Y'
>         (Node 'S' Empty Empty)
>         (Node 'A' Empty Empty)
>       )
>     )
>     (Node 'L'
>       (Node 'W' -- Here is our 'W' !
>         (Node 'C' Empty Empty)
>         (Node 'R' Empty Empty)
>       )
>       (Node 'A'
>         (Node 'A' Empty Empty)
>         (Node 'C' Empty Empty)
>       )
>     )

Say, we want to change 'W' into a 'P'.

One way would be to pattern match on our tree until we find the element, by first going right and then left.

> changeToP' :: Tree Char -> Tree Char
> changeToP' (Node x 
>             l
>             (Node y 
>               (Node _ m n)
>               rest
>             )
>           )
>           =
>           (Node x 
>             l
>             (Node y
>               (Node 'P' m n)
>               rest
>             )
>           )

  *ZIP_15> freeTree
  Node 'P' 
    (Node 'O' 
      (Node 'L' 
        (Node 'N' Empty Empty) 
        (Node 'T' Empty Empty)
      ) 
        (Node 'Y' 
          (Node 'S' Empty Empty) 
          (Node 'A' Empty Empty)
        )
    ) 
    (Node 'L' 
      (Node 'W'                  -- Here is our target.
        (Node 'C' Empty Empty) 
        (Node 'R' Empty Empty)
      ) 
      (Node 'A' 
        (Node 'A' Empty Empty) 
        (Node 'C' Empty Empty)
      )
    )
  *ZIP_15> changeToP' freeTree 
  Node 'P' 
    (Node 'O' 
      (Node 'L' 
        (Node 'N' Empty Empty) 
        (Node 'T' Empty Empty)
      ) 
      (Node 'Y' 
        (Node 'S' Empty Empty) 
        (Node 'A' Empty Empty)
      )
    ) 
    (Node 'L' 
      (Node 'P'                  -- Here we change.
        (Node 'C' Empty Empty) 
        (Node 'R' Empty Empty)
      ) 
      (Node 'A' 
        (Node 'A' Empty Empty) 
        (Node 'C' Empty Empty)
      )
    )

Not only is this rather ugly, it's also kind of confusing.
Here is our freetree:

         P
        / \
       /   \
      /     \
     /       \
    O         L
   / \       / \
  /   \     /   \
 L     Y   W     A
 ^     ^   ^     ^
N T   S A C R   A C
^ ^   ^ ^ ^ ^   ^ ^

where I have omitted the last Empty's.

The following is the pattern matching in changeToP' function

         x
        / \
       /   \
      /     \
     /       \
    l         y
             / \
            /   \
           _    rest   -- \_ -> 'P'

Is there a better way of doing this?
How about if we make our function take a tree along with a list of directions.
The direction will be either L(left) or R(right).
We'll change the element that we arrive at by following the supplied direction.

> data Direction = L | R
>                deriving (Show, Eq)
> type Directions = [Direction]
>
> changeToP :: Directions -> Tree Char -> Tree Char
> changeToP (L:ds) (Node x l r) = 
>   Node x (changeToP ds l) 
>          r
> changeToP (R:ds) (Node x l r) =
>   Node x l
>          (changeToP ds r)
> changeToP [] (Node _ l r) = Node 'P' l r

Now I added Eq in Tree declaration.

  *ZIP_15> changeToP' freeTree 
  Node 'P' (Node 'O' (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)) (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty))) (Node 'L' (Node 'P' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))
  *ZIP_15> changeToP [R,L] freeTree == changeToP' freeTree
  True

To avoind printing out the whole tree, let's make a function that takes a list of directions and tells us the element at the destination:

> elemAt :: Directions -> Tree a -> a
> elemAt (L:ds) (Node _ l _) = elemAt ds l
> elemAt (R:ds) (Node _ _ r) = elemAt ds r
> elemAt []     (Node x _ _) = x

  *ZIP_15> elemAt [R,L] freeTree 
  'W'
  *ZIP_15> let newTree = changeToP [R,L] freeTree
  *ZIP_15> elemAt [R,L] newTree
  'P'

In these functions, the list of directions acts as a sort of focus, because it pinpoints one exact subtree of our tree.

While this technique may seem cool (I don't think so!), it can be rather inefficient, especially if we want to repeatedly change elements.
If we want to change two elements that are close to each other, we need to start from the root of the tree and walk all the way to the bottom again.

A Trail of Breadcrumbs
For focusing on a subtree, we want something better than just a list of directions that we always follow from the root of our tree.
Would it help if we started at the root of the tree and moved either left or right one step at a time, leaving "breadcrumbs" along the way?

> type Breadcrumbs = [Direction] -- reversed order

Here is a function that takes a tree and some breadcrumbs and moves to the left subtree while adding L to the head of the list that represents our breadcrumbs:

> goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
> goLeft (Node _ l _, bs) = (l, L:bs)
>
> goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
> goRight (Node _ _ r, bs) = (r, R:bs)

> (-:) :: a -> (a -> b) -> b
> x -: f = f x

I added Eq, and

  *ZIP_15> (freeTree, []) -: goRight -: goLeft == (freeTree, []) -: goRight -: goLeft
  True

Going Back Up
