M_06.lhs

Chapter 06: Modules

> module M_06 where

> import Data.List
> -- import qualified Data.List as L
>
> import Data.Char
> -- import must be done before ANY functions, so imports usually at the top of the file.
>
> import qualified Data.Map as Map

> numUniques :: (Eq a) => [a] -> Int
> numUniques = length . nub

Counting words

> wordNums :: String -> [(String, Int)]
> wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

*M_06> words "tick tack tick teck tick tack"
["tick","tack","tick","teck","tick","tack"]
*M_06> sort it
["tack","tack","teck","tick","tick","tick"]
*M_06> group it
[["tack","tack"],["teck"],["tick","tick","tick"]]
*M_06> map length it
[2,1,3]
*M_06> head ["tack","tack"]
"tack"

Needle in the haystack
For our next mission, should we choose to accept it, we will make a function that takes two lists and tells us if the first list is wholly contained anywhere in the second list.

> isIn :: (Eq a) => [a] -> [a] -> Bool
> needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

*M_06> tails "party"
["party","arty","rty","ty","y",""]
*M_06> any (isPrefixOf "art") it
True

Caesar Cipher Salad

> encode :: Int -> String -> String
> encode offset msg = map (\c -> chr $ ord c + offset) msg

> decode :: Int -> String -> String
> -- decode shift msg = map (\c -> chr $ ord c - shift) msg
> decode shift msg = encode (negate shift) msg

On strict left folds 
lazy = not strict

Let's find some cool numbers
What's the first natural number s.t. the sum of its digits equals to 40?

> digitSum :: Int -> Int
> digitSum = sum . map digitToInt . show

> firstTo :: Int -> Maybe Int
> firstTo n = find (\x -> digitSum x == n) [1..]

Mapping keys to values
Almost as good: association lists so called dictionaries

> phoneBook = 
>   [("betty", "555-2938")
>   ,("bonnie", "452-2928")
>   ,("patsy", "493-2928")
>   ,("lucille", "205-2928")
>   ,("wendy", "939-8282")
>   ,("penny", "853-2492")
>   ]

> findKey' :: (Eq k) => k -> [(k,v)] -> v
> findKey' key xs = snd . head . filter (\(k,v) -> key == k) $ xs

The above implement does not work if the result is empty:

> findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
> -- findKey key [] = Nothing
> -- findKey key ((k,v):xs)
> --   |key == k  = Just v
> --   |otherwise = findKey key xs
> findKey key xs = -- classical foldr pattern
>   foldr (\(k,v) acc -> if key == k then Just v
>                                    else acc) Nothing xs

The above implement does not work if a key shares some phone numbers, e.g. see phoneBook'' below.

Enter Data.Map

*M_06> :type Map.fromList
Map.fromList :: Ord k => [(k, a)] -> Map.Map k a
fromList in Data.Map takes an association list and returns a map with the same associations.
It needs the keys to be an instance of Ord so it can arrange and access them more efficiently.

> phoneBook' :: Map.Map String String
> phoneBook' = Map.fromList $ phoneBook

*M_06> :type Map.lookup
Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a
*M_06> Map.lookup "betty" phoneBook'
Just "555-2938"
*M_06> Map.lookup "betty'" phoneBook'
Nothing

*M_06> :type Map.insert
Map.insert :: Ord k => k -> a -> Map.Map k a -> Map.Map k a
*M_06> Map.lookup "grace" phoneBook'
Nothing
*M_06> let newBook = Map.insert "grace" "341-9021" phoneBook'
*M_06> Map.lookup "grace" phoneBook'
Nothing
*M_06> Map.lookup "grace" newBook
Just "341-9021"
*M_06> newBook 
fromList [("betty","555-2938"),("bonnie","452-2928"),("grace","341-9021"),("lucille","205-2928"),("patsy","493-2928"),("penny","853-2492"),("wendy","939-8282")]
*M_06> phoneBook'
fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928"),("patsy","493-2928"),("penny","853-2492"),("wendy","939-8282")]

> string2digits :: String -> [Int]
> string2digits = map digitToInt . filter isDigit

> phoneBook'' =
>   [("betty", "555-2938")
>   ,("betty", "342-2492")
>   ,("bonnie", "452-2928")
>   ,("patsy", "493-2928")
>   ,("patsy", "943-2929")
>   ,("patsy", "827-9162")
>   ,("lucille", "205-2928")
>   ,("wendy", "939-8282")
>   ,("penny", "853-2492")
>   ,("penny", "555-2111")
>   ]
>
> phoneBookToMap' :: (Ord k) => [(k, String)] -> Map.Map k String
> phoneBookToMap' xs = Map.fromListWith add xs
>   where add num1 num2 = num1 ++ ", " ++ num2
>
> phoneBookToMap :: (Ord k) => [(k,a)] -> Map.Map k [a]
> phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs

*M_06> Map.lookup "patsy" $ phoneBookToMap phoneBook''
Just ["827-9162","943-2929","493-2928"]

