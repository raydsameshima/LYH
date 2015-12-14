SIF_03.lhs

Chapter 03: Syntax In Functions

> module SIF_03 where

Pattern Matching
When making patterns, we should always include a catchall pattern at the end so our program doesn't crash if we get some unexpected input.

Pattern Matching with Lists and List comprehensions

> tupleOfVectors = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
> normSquare2D xs = [a^2+b^2 | (a,b) <- xs]

*SIF_03> :type normSquare2D 
normSquare2D :: Num t => [(t, t)] -> [t]
*SIF_03> normSquare2D tupleOfVectors 
[10,25,20,34,61,10]
*SIF_03> (map (sqrt . fromInteger) it ) :: [Float]
[3.1622777,5.0,4.472136,5.8309517,7.81025,3.1622777]

> norm2D :: [(Integer, Integer)] -> [Float]
> norm2D xs = map (sqrt . fromInteger) (normSquare2D xs)

As-patterns
~ allow you to break up an item according to a pattern, while still keeping a reference to the entire original item.

> firstLetter :: String -> String
> firstLetter ""         = "Empty!"
> firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

> firstLetter' (x:xs) = "The first letter of " ++ (x:xs) ++ " is " ++ [x]

http://www.sampou.org/haskell/tutorial-j/patterns.html
アズパターン

場合によってはパターンに名前がついていれば、等式の右辺で使えて便利なこと があります。たとえば、リストの第一要素を複製する関数はつぎのように書けま す。 

f (x:xs)                = x:x:xs

(「:」は右結合性があることを思いだしてください。) x:xs というパターンが左辺と右辺の両方にあります。可読性向上のためには x:xs は一度だけ書くほうがよく、アズパターン ( as-pattern )を使ってこれを達成します。(このパターンを使うもうひ とつの利点は、素朴な実装では、照合が成功した値を再利用せずに、x:xs を再構築することになるからです。) 

f s@(x:xs)             = x:s

技術的な観点からいうと、アズパターンは照合に常に成功します。ただし、 サブパターン(この場合は、x:xs )はもちろん照合が成功しないこともあり ます。

Guards, Guards!
where?!

> bmiTell :: Double -> Double -> String
> bmiTell weight height
>   | bmi <= 18.5 = "You're underweight"
>   | bmi <= 25.0 = "Normal"
>   | bmi <= 30.0 = "Fat!"
>   | otherwise   = "Are you a whale?"
>   where bmi = weight /(height^2)

The variables in the where section of a function are visible only to that function.
Also, where bindings are not shared across function bodies of different patterns!

greet :: String -> String
greet "Juan"     = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name       = badGreeting ++ " " ++ name
  where niceGreeting = "Hello! So very nice to see you,"
        badGreeting  = "Oh! Pfft. It's you."

This function won't work as written, because where bindings are NOT shared across function bodies of different patterns, only the last function body sees the greetings defined by the where binding.

> greet :: String -> String
> greet "Juan"     = niceGreeting ++ " Juan!"
> greet "Fernando" = niceGreeting ++ " Fernando!"
> greet name       = badGreeting ++ " " ++ name
>
> niceGreeting = "Hello! So very nice to see you,"
> badGreeting  = "Oh! Pfft. It's you."

*SIF_03> greet "Juan"
"Hello! So very nice to see you, Juan!"
*SIF_03> greet "Fernando"
"Hello! So very nice to see you, Fernando!"
*SIF_03> greet "Ray"
"Oh! Pfft. It's you. Ray"

Let It Be
let is an expression.
The function for the surface area of cylinder (radius, height):

> cylinder r h =
>   let sideArea = 2 * pi * r * h
>       topArea  = pi * r^2
>   in sideArea + 2 * topArea

let in List Comprehensions

> fatPeoples xs = [bmi | (w, h) <- xs, let bmi = w/(h^2), bmi > 25.0]

case Expressions

> head' :: [a] -> a
> head' xs = case xs of []    -> error "No head for empty lists!"
>                       (x:_) -> x

> describeList :: [a] -> String
> describeList ls = 
>   "The list is " 
>     ++ case ls of []  -> "empty."
>                   [x] -> "a singleton list."
>                   _   -> "a longer list."

Since the guard is syntax sugar of case expresseion, we can write above as

describeList ls = "The list is " ++ what ls
  where what []  = "empty."
        what [x] = "a singleton list."
        what xs  = "a longer list."



