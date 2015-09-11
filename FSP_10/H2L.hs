-- H2L.hs
-- Heathrow to London
-- chapter 10

module H2L where

{-
The algorithm is the following:
1. We see what the best path to the next crossroads on main road A is.
The two options are going directly forward or starting at the opposite road, going forward and then crossing over.
We remember the cost and the path.
  (A = 50), (BC = 40)

2. We use the same method to find the best path to the next crossroads on main road B and remember that.
  (B = 10), (AC = 80)

3. We see if the path to the next crossroads on A takes less time if we go from the previous A crossroads or if we go from the previous B crossroads and then cross over.
  (AA = 55), (BCA = 45)
  
We remember the quicker path.
  for A_2 (BCA = 45)

We do the same for the crossroads opposite of it.
  for B_2 (BCAC = 65)

4. We do this for every section until we reach the end.

5. Once we've reached the end, the quicker of the two path that we have is our optimal path.
-}

-- representin the road system in haskell

data Section = Section { getA :: Int
                       , getB :: Int
                       , getC :: Int
                       } deriving (Show)
type RoadSystem = [Section]

{-
getA :: Section -> Int
Section :: Int -> Int -> Int -> Section
-}

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30
                   , Section 5 90 20
                   , Section 40 2 25
                   , Section 10 8 0
                   ]

-- writing the optimal path function

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

-- optimalPath :: RoadSystem -> Path

{-
We're going to need to walk over the list with the sections from left right and keep the optimal path on A and optimal path on B as we go along.
We'll accumulate the best path as we walk over the list, left to right: 
  foldl
-}

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let timeA = sum $ map snd pathA
      timeB = sum $ map snd pathB
      forwardTimeToA = timeA + a
      crossTimeToA   = timeB + b + c
      forwardTimeToB = timeB + b
      crossTimeToB   = timeA + c + a
      newPathToA = if forwardTimeToA <= crossTimeToA
                      then (A, a) : pathA
                      else (C, c) : (B, b) : pathB
      newPathToB = if forwardTimeToB <= crossTimeToB
                      then (B, b) : pathB
                      else (C, c) : (A, a) : pathA
  in (newPathToA, newPathToB)

roadStep' = roadStep ([],[]) 
{-
*H2R> roadStep' (Section 50 10 30)
([(C,30),(B,10)],[(B,10)])
*H2R> roadStep' (Section 5 90 20)
([(A,5)],[(C,20),(A,5)])
*H2R> roadStep' (Section 40 2 25)
([(C,25),(B,2)],[(B,2)])
*H2R> roadStep' (Section 10 8 0)
([(C,0),(B,8)],[(B,8)])
-}

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
  in if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then reverse bestAPath
        else reverse bestBPath

-- getting a road system fro the input

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _  = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
