-- palindromes.hs

respondPalindromes :: String -> String
respondPalindromes =
  {- unlines .
  map (\xs -> if isPal xs
                then "palindrome"
                else "not a palindrome") .
  lines
  -}
  unlines . map wetherPalOrNot . lines
    where wetherPalOrNot :: String -> String
          wetherPalOrNot xs = if isPal xs
                   then "palindrome"
                   else "not a palindrome"

isPal :: String -> Bool
isPal xs = xs == reverse xs

main = interact respondPalindromes
