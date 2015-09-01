MOOTATC_07.lhs

Making Our Own Types And Type Classes

> -- module MOOTATC_07 where
> module MOOTATC_07 
> ( Point(..) -- If you want to export all the value constructors...
> , Shape(..)
> , area
> , nudge
> , baseCircle
> , baseRectangle
> , vmult 
> , Tree(..)
> )where

> import qualified Data.Map as Map

Shaping Up

> -- data Shape = Circle Float Float Float 
> --            | Rectangle Float Float Float Float
> --   deriving (Show)

> -- area :: Shape -> Float
> -- area (Circle _ _ r) = pi * r^2
> -- area (Rectangle x1 y1 x2 y2) = abs $ (x2-x1)*(y2-y1)

Improving Shape with the Point Data Type

> data Point = Point Float Float deriving (Show)
> data Shape = Circle Point Float 
>            | Rectangle Point Point 
>   deriving (Show)

Notice that when defining a point, we used the same name for the data type and the value constructor.
This has no special meaning, although it's common if there's only one value constructor.

> area :: Shape -> Float
> area (Circle _ r) = pi * r^2
> area (Rectangle (Point x1 y1) (Point x2 y2)) = abs $ (x2-x1)*(y2-y1)

*MOOTATC_07> area $ Rectangle (Point 0 0) (Point 100 100)
10000.0
*MOOTATC_07> area $ Circle (Point 0 0) 24
1809.5574

> nudge :: Shape -> Float -> Float -> Shape -- paralell displacement
> nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
> nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b
>   = Rectangle (Point x1' y1') (Point x2' y2')
>       where (x1', y1', x2', y2') = (x1+a, y1+b, x2+a, y2+b) 

> baseCircle :: Float -> Shape
> baseCircle r = Circle (Point 0 0) r

> baseRectangle :: Float -> Float -> Shape
> baseRectangle width height 
>   = Rectangle (Point 0 0) (Point width height)

Record Syntax

> data Person = Person { firstName :: String -- fisrtName :: Person -> String
>                      , lastName :: String
>                      , age :: Int
>                      , height :: Float
>                      , phoneNumber :: String
>                      , flavor :: String
>                      } deriving (Show, Eq)

> data Car = Car { company :: String
>                , model :: String
>                , year :: Int
>                } deriving (Show)

Vector von Doom

> data Vector a = Vector a a a deriving (Show)

(Vector a) type consists of a value of Vector a a a)

...it's very important to distinguish between the type constructor and the value constructor.
When declaring a data type, the part before = is the type constructor, and the constructors after it (possibly separated by | characters) are value constructors.

> vplus :: (Num a) => Vector a -> Vector a -> Vector a
> (Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)
>
> dotProd :: (Num a) => Vector a -> Vector a -> a
> (Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n
>
> vmult :: (Num a) => Vector a -> a -> Vector a
> (Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

Derived Instances
Equating People

> data Person2 = Person2 { firstName2 :: String
>                        , lastName2 :: String
>                        , age2 :: Int
>                        } deriving (Eq, Show, Read)

> mysteryDude = "Person2 { firstName2 =\"Michael\"" ++
>                      ", lastName2 =\"Diamond\"" ++
>                      ", age2 = 43}" 

*MOOTATC_07> mysteryDude 
"Person2 { firstName2 =\"Michael\", lastName2 =\"Diamond\", age2 = 43}"
*MOOTATC_07> read mysteryDude 
*** Exception: Prelude.read: no parse
*MOOTATC_07> read mysteryDude :: Person2
Person2 {firstName2 = "Michael", lastName2 = "Diamond", age2 = 43}

> data Day = Monday
>          | Tueseday | Wednesday | Thursday | Friday | Saturday | Sunday
>          deriving (Eq, Ord, Show, Read, Bounded, Enum) -- all the derivable type classes

Type Synonyms

> type PhoneNumber = String
> type Name = String
> type PhoneBook = [(Name, PhoneNumber)]
>
> inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
> inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

Go Left, Then Right

Maybe a is used to represent the results of computations that could have failed.
But sometimes, Maybe a isn't good enough, because Nothing doesn't convey much information other than that something has failed.
However, if when we're interested in how or why some function failed, we usually use the result type of Either a b:
data Either a b = Left a
                | Right b deriving (Eq, Ord, Read, Show)

> data LockerState = Taken 
>                  | Free deriving (Show, Eq)
> type Code = String
> type LockerMap = Map.Map Int (LockerState, Code)
> type LockerNum = Int
>
> lockerLookup :: LockerNum -> LockerMap -> Either String Code
> lockerLookup num map = case Map.lookup num map of
>   Nothing -> Left $ "Locker " ++ show num ++ " doesn't exists!"
>   Just (state, code) 
>           -> if state /= Taken 
>              then Right code
>              else Left $ "Locker " ++ show num ++ " is already taken!"

> lockers :: LockerMap
> lockers = Map.fromList
>   [(100, (Taken, "ZD39I"))
>   ,(101, (Free,"JAH3I"))
>   ,(103,(Free, "IQSA9"))
>   ,(105,(Free, "QOTSA"))
>   ,(109,(Taken, "893JJ"))
>   ,(110,(Taken, "99292"))
>   ]

Recursive Data Structure

data List a = Empty
            | Cons a (List a) deriving (Show, Read, Eq, Ord)

Improvin Our List
We can define functions to be automatically infix by naming them using only special charcters.
We can also do the same with constructors, since they're just functions that return a date type.
There is one restriction however: Infix constructors must begin with a colon.

> infixr 5 :-:
> data List a = Empty 
>             | a :-: (List a) deriving (Show, Read, Eq, Ord)

First, notice a new systactic construct: the fixity declaration, which is the line above our data declaration.
When we define functions as operators, we can use that to give them a fixity (but we don't have to).

> infixr 4 ^++
> (^++) :: List a -> List a -> List a
> Empty ^++ ys = ys
> (x :-: xs) ^++ ys = x :-: (xs ^++ ys)

Let's Plant a Tree
binary search trees

> data Tree a = EmptyTree
>             | Node a (Tree a) (Tree a) deriving (Show)

> singleton :: a -> Tree a
> singleton x = Node x EmptyTree EmptyTree
>
> treeInsert :: (Ord a) => a -> Tree a -> Tree a
> treeInsert x EmptyTree = singleton x
> treeInsert x (Node a left right)
>   | x == a = Node x left right
>   | x < a  = Node a (treeInsert x left) right
>   | x > a  = Node a left (treeInsert x right)
>
> treeElem :: (Ord a) => a -> Tree a -> Bool
> treeElem x EmptyTree = False
> treeElem x (Node a left right)
>   | x == a = True
>   | x < a  = treeElem x left
>   | x > a  = treeElem x right

*MOOTATC_07 Data.Map> let nums = [8,6,4,1,7,3,5]
*MOOTATC_07 Data.Map> let numsTree = Prelude.foldr treeInsert EmptyTree nums
*MOOTATC_07 Data.Map> numsTree 
Node 5 
  (Node 3 
    (Node 1 EmptyTree EmptyTree)
    (Node 4 EmptyTree EmptyTree)
  ) 
  (Node 7 
    (Node 6 EmptyTree EmptyTree)
    (Node 8 EmptyTree EmptyTree)
  )
*MOOTATC_07 Data.Map> treeInsert 2 numsTree 
Node 5 
  (Node 3 
    (Node 1 EmptyTree 
      (Node 2 EmptyTree EmptyTree)
    ) 
      (Node 4 EmptyTree EmptyTree)
    ) 
  (Node 7 
    (Node 6 EmptyTree EmptyTree) 
    (Node 8 EmptyTree EmptyTree)
  )

A Traffic Light Data Type

> data TrafficLight = Red | Yellow | Green
> instance Eq TrafficLight where -- ...write some instances by hand
>   Red == Red       = True
>   Green == Green   = True
>   Yellow == Yellow = True
>   _ == _           = False
> instance Show TrafficLight where
>   show Red    = "Red light"
>   show Green  = "Green light"
>   show Yellow = "Yellow light"

Subclassing
Eq type class declaration is 
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)

Num is type subclass:
class (Eq a) => Num a where

Parameterized Types As Instances of Type Classes
instance (Eq m) => Eq (Maybe m) where
  Just x == Just y   = x == y
  Nothing == Nothing = True
  _ == _             = False
This is the declaration that Maybe m is an instance of Eq type class, where m is an instance of Eq. 

A Yes-No Type Class ~like some weakly typed languages

> class YesNo a where
>   yesno :: a -> Bool
> instance YesNo Int where
>   yesno 0 = False
>   yesno _ = True
> instance YesNo [a] where
>   yesno [] = False
>   yesno _  = True
> instance YesNo Bool where
>   yesno = id
> instance YesNo (Maybe a) where
>   yesno (Just _) = True
>   yesno Nothing  = False
> instance YesNo (Tree a) where
>   yesno EmptyTree = False
>   yesno _         = True
> instance YesNo TrafficLight where
>   yesno Red = False
>   yesno _   = True
>
> yesnoIf :: (YesNo y) => y -> a -> a-> a
> yesnoIf yesnoVal yesResult noResult =
>   if yesno yesnoVal
>     then yesResult
>     else noResult

The Functor Type Class

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  
That is, in the category of Hask whose objects are types and arrows are the functions between types, f is an instance of functor and fmap function is a generalized map function:

instance Functor [] where
  fmap = map

[] is a type constructor that takes one type and can create a concrete type, e.g., for lists, fmap is just map.

Prelude> :kind []
[] :: * -> *

Maybe As a Functor

instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing  = Nothing 

> instance Functor Tree where
>   fmap f EmptyTree = EmptyTree
>   fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

*MOOTATC_07> fmap (*3) (foldr treeInsert EmptyTree [5,7,2])
Node 6 EmptyTree 
       (Node 21 (Node 15 EmptyTree EmptyTree) 
                EmptyTree
       )

Either a As a Functor
like a partial application

instance Functor (Either a) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x)  = Left x

