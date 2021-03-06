ZIP_15.lhs

Chapter 15 ZIPPER

> module ZIP_15 where

> import Data.List (break) -- for filesystem

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
>       (Node 'W'                -- Here is our 'W' !
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
> changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
> changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
> changeToP []     (Node _ l r) = Node 'P' l r

Now I added Eq in Tree declaration, and to see what' going on:

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

> type Breadcrumbs' = [Direction] -- reversed order

Here is a function that takes a tree and some breadcrumbs and moves to the left subtree while adding L to the head of the list that represents our breadcrumbs:

> goLeft' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
> goLeft' (Node _ l _, bs) = (l, L:bs)
>
> goRight' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
> goRight' (Node _ _ r, bs) = (r, R:bs)

> (-:) :: a -> (a -> b) -> b
> x -: f = f x

I added Eq, and

  *ZIP_15> (freeTree, []) -: goRight' -: goLeft' == (freeTree, []) -: goRight' -: goLeft'
  True

Going Back Up
Let's modify our breadcrumbs so that they also contain information about everything that we previously ignored when moving left and right.
Instead of Direction, we'll make a new data type:

> data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a)
>              deriving (Show, Eq)

Now, instead of just L, we have a LeftCrumb, which also contains the element in the node that we moved from and the right tree that we didn't visit.

These breadcrumbs now contain all the data needed to re-create the tree that we walked through.
So, instead of just being normal breadcrumbs, they're more like floppy disks that we leave as we go along, because they contain a lot more information than just the direction that we took.

In essence, every breadcrumb is now like a tree node with a hole in it.

Let's also change our Breadcrumbs type synonym to reflect this:

> type Breadcrumbs a = [Crumb a]

Next up, we also need to modify the goLeft and goRIght functions to store information about the path that we didn't take in our breadcrumbs, instead of ignoring that information as they did before.

> -- goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
> -- goLeft (Node x l r, bs) = (l, LeftCrumb x r :bs)
> -- goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
> -- goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

Note that these functions assume that the current tree that's under focus isn't Empty.
Pattern match will fail if they are empty tree.

We were previously able to go left and right.
What we have now in the ability to actually go back up by remembering stuff about the parent nodes and paths that we didn't visit.
Here is the goUp function:

> -- goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a) 
> -- goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
> -- goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

Note that this function causes an error if we're already at the top of a tree and we want to move up.
Later on, we'll use the Maybe monad to represent possible failure when moving focus.

With a pair of Tree a and Breadcrumbs a, we have all the information we need to rebuild the whole tree, and we have a focus on a subtree.
This scheme enables us to easily move up, left and right.
A pair that contains a focused part of a data structure and its surroundings is called a zipper, because moving our focus up and down the data structure resembles the operation of a zipper on a pair of pants.

> type Zipper a = (Tree a, Breadcrumbs a) -- Focus is also a cool synonym

Manipulating Trees Under Focus
Now we can move up and down (and of course left and right), let's make a function that modifies the element in the root of the subtree on which the zipper is focusing:

> modify :: (a -> a) -> Zipper a -> Zipper a
> modify f (Node x l r, bs) = (Node (f x) l r, bs)
> modify f (Empty,      bs) = (Empty,          bs)

If we're focusing on a node, we modify its root element with the function f.

  *ZIP_15> let newFocus = modify (\_ -> 'P') $ goRight $ goLeft $ (freeTree, [])
  *ZIP_15> freeTree 
  Node 'P' (Node 'O' (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)) (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty))) (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))
  *ZIP_15> newFocus 
  (Node 'P' (Node 'S' Empty Empty) (Node 'A' Empty Empty),[RightCrumb 'O' (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)),LeftCrumb 'P' (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))])
  *ZIP_15> :type it
  it :: Zipper Char

We go left, then right, and then modify the root element by replacing it with a 'P'.
This reads even better if we use (-:) function:
(I have added an Eq)

  *ZIP_15> let newFocus = modify (\_ -> 'P') $ goRight $ goLeft $ (freeTree, [])
  *ZIP_15> let newerFocus = (freeTree, []) -: goLeft -: goRight -: modify (\_ -> 'P')
  *ZIP_15> newFocus == newerFocus 
  True

We can then move up if we want and replace an element with a mysterious 'X':

  *ZIP_15> let newFocus = modify (\_ -> 'P') $ goRight $ goLeft $ (freeTree, [])
  *ZIP_15> let newerFocus = (freeTree, []) -: goLeft -: goRight -: modify (\_ -> 'P')
  *ZIP_15> newFocus == newerFocus 
  True

Of course we have

  *ZIP_15> let newerFocus2 = modify (\_ -> 'X') (goUp newFocus)
  *ZIP_15> newFocus2 == newerFocus2 
  True

Moving up is easy because the breadcrumbs that re leave form the part of the data structure that we're not focusing on, but it's inverted, sort of like turning a sock inside out.
That's why when we want to move up, we don't need to start from the root and make our way down.
We just take the top of our inverted tree, thereby uninverting a part of it and adding it to our focus.
  
> attach :: Tree a -> Zipper a -> Zipper a
> attach t (_, bs) = (t,bs)
  
  *ZIP_15> freeTree 
  Node 'P' (Node 'O' (Node 'L' (Node 'N' Empty Empty) 
                               (Node 'T' Empty Empty)) 
                     (Node 'Y' (Node 'S' Empty Empty) 
                               (Node 'A' Empty Empty))) 
           (Node 'L' (Node 'W' (Node 'C' Empty Empty) 
                               (Node 'R' Empty Empty)) 
                     (Node 'A' (Node 'A' Empty Empty) 
                               (Node 'C' Empty Empty)))


  *ZIP_15> let farLeft = (freeTree, []) -: goLeft -: goLeft -: goLeft
  *ZIP_15> farLeft 
  (Node 'N' Empty Empty,
  [
    LeftCrumb 'L' (Node 'T' Empty Empty)
   ,LeftCrumb 'O' (Node 'Y' (Node 'S' Empty Empty) 
                            (Node 'A' Empty Empty))
   ,LeftCrumb 'P' (Node 'L' (Node 'W' (Node 'C' Empty Empty) 
                                      (Node 'R' Empty Empty)) 
                            (Node 'A' (Node 'A' Empty Empty) 
                                      (Node 'C' Empty Empty)))])

  *ZIP_15> let newFocus = farLeft -: attach (Node 'Z' Empty Empty)
  *ZIP_15> newFocus 
  (
  Node 'Z' Empty Empty,
  [
     LeftCrumb 'L' (Node 'T' Empty Empty)
    ,LeftCrumb 'O' (Node 'Y' (Node 'S' Empty Empty) 
                             (Node 'A' Empty Empty))
    ,LeftCrumb 'P' (Node 'L' (Node 'W' (Node 'C' Empty Empty) 
                                       (Node 'R' Empty Empty)) 
                             (Node 'A' (Node 'A' Empty Empty) 
                             (Node 'C' Empty Empty)))
  ]
  )

Going Straight to the Top, Where the Air is Fresh and Clean!

> -- topMost :: Zipper a -> Zipper a
> -- topMost (t,[]) = (t, [])     -- terminal condition
> -- topMost z = topMost $ goUp z -- recursion

  *ZIP_15> (freeTree, []) -: goLeft -: goLeft -: goRight
  (Node 'T' Empty Empty,[RightCrumb 'L' (Node 'N' Empty Empty),LeftCrumb 'O' (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty)),LeftCrumb 'P' (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))])
  *ZIP_15> topMost it
  (Node 'P' (Node 'O' (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)) (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty))) (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty))),[])

Focusing on Lists
Zippers can be used with pretty much any data structure, so it's no surprise that they work with sublists of lists.

A list can be seen as a tree with no sub tree (a straight tree).

> type ListZipper a = ([a], [a])
>
> goForward :: ListZipper a -> ListZipper a
> goForward (x:xs, bs) = (xs, x:bs)
> goBack :: ListZipper a -> ListZipper a
> goBack (xs, b:bs) = (b:xs, bs)

A very Simple Filesystem

> type Name = String
> type Data = String
> data FSItem = File Name Data
>             | Folder Name [FSItem]
>             deriving (Show)

> myDisk :: FSItem
> myDisk =
>   Folder "root"
>     [ File "goat_yelling_like_man.wmv" "baaaaaa"
>     , File "pope_time.avi" "god bless"
>     , Folder "pics"
>         [ File "aple_throwing_up.jpg" "bleargh"
>         , File "watermelon_smash.gif" "smash!!"
>         , File "skull_man(scary).bmp" "Yikes!"
>         ]
>     , File "dijon_poupon.doc" "best mustard"
>     , Folder "programs"
>         [ File "fartwizard.exe" "10gotofart"
>         , File "owl_bandit.dmg" "mov eax, h00t"
>         , File "not_a_virus.exe" "really not a virus"
>         , Folder "source code"
>             [ File "best_hs_prog.hs" "main = print (fix error)"
>             , File "random.hs" "main = print 4"
>             ]
>         ]
>     ]

Making a Zipper for Our Filesystem

> data FSCrumb = FSCrumb Name [FSItem] [FSItem]
>              deriving (Show)
> type FSZipper = (FSItem, [FSCrumb])
>
> fsUp :: FSZipper -> FSZipper
> fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)
>
> fsTo :: Name -> FSZipper -> FSZipper
> fsTo name (Folder folderName items, bs) =
>   let (ls, item:rs) = break (nameIs name) items
>   in  (item, FSCrumb folderName ls rs:bs)
>
> nameIs :: Name -> FSItem -> Bool
> nameIs name (Folder folderName _) = name == folderName
> nameIs name (File   fileName   _) = name == fileName

  *ZIP_15> :type break 
  break :: (a -> Bool) -> [a] -> ([a], [a])

  *ZIP_15> (myDisk, []) -: fsTo "pics" -: fst
  Folder "pics" [File "aple_throwing_up.jpg" "bleargh",File "watermelon_smash.gif" "smash!!",File "skull_man(scary).bmp" "Yikes!"]

  *ZIP_15> let newFocus = (myDisk, []) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"
  *ZIP_15> fst newFocus 
  File "skull_man(scary).bmp" "Yikes!"

  *ZIP_15> let newFocus = (myDisk, []) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"
  *ZIP_15> let newFocus2 = newFocus -: fsUp -: fsTo "watermelon_smash.gif"
  *ZIP_15> fst newFocus2
  File "watermelon_smash.gif" "smash!!"

Manipulating a Filesystem

> fsRename :: Name -> FSZipper -> FSZipper
> fsRename newName (Folder name items, bs) = (Folder newName items, bs)
> fsRename newName (File name dat, bs) = (File newName dat, bs)

  *ZIP_15> let newFocus = (myDisk, []) -: fsTo "pics" -: fsRename "cspi" -: fsUp
  *ZIP_15> newFocus -: fst 
  Folder "root" [File "goat_yelling_like_man.wmv" "baaaaaa",File "pope_time.avi" "god bless",Folder "cspi" [File "aple_throwing_up.jpg" "bleargh",File "watermelon_smash.gif" "smash!!",File "skull_man(scary).bmp" "Yikes!"],File "dijon_poupon.doc" "best mustard",Folder "programs" [File "fartwizard.exe" "10gotofart",File "owl_bandit.dmg" "mov eax, h00t",File "not_a_virus.exe" "really not a virus",Folder "source code" [File "best_hs_prog.hs" "main = print (fix error)",File "random.hs" "main = print 4"]]]

> fsNewFile :: FSItem -> FSZipper -> FSZipper
> fsNewFile item (Folder folderName items, bs) = (Folder folderName (item:items), bs)

Watch Your Step

> goLeft, goRight :: Zipper a -> Maybe (Zipper a)
> goLeft (Node x l r, bs)  = Just (l, LeftCrumb x r:bs)
> goLeft (Empty, _)        = Nothing
> goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
> goRight (Empty, _)       = Nothing

  *ZIP_15> goLeft (Empty, [])
  Nothing
  *ZIP_15> goLeft (Node 'A' Empty Empty, [])
  Just (Empty,[LeftCrumb 'A' Empty])
  *ZIP_15> it >>= goLeft
  Nothing

> goUp :: Zipper a -> Maybe (Zipper a)
> goUp (t, LeftCrumb x r:bs)  = Just (Node x t r, bs)
> goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
> goUp (_, [])                = Nothing

  *ZIP_15> let coolTree = Node 1 Empty (Node 3 Empty E
  EQ      Either  Empty   Enum    Eq
  *ZIP_15> let coolTree = Node 1 Empty (Node 3 Empty Empty )
  *ZIP_15> return (coolTree, []) >>= goRight
  Just (Node 3 Empty Empty,[RightCrumb 1 Empty])
  *ZIP_15> it >>= goRight
  Just (Empty,[RightCrumb 3 Empty,RightCrumb 1 Empty])
  *ZIP_15> it >>= goRight
  Nothing
