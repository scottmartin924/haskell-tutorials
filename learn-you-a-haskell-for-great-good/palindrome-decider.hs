-- Takes in string and returns if string is a palindrome, false otherwise
isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse xs

-- Basically just checks if palindrome but ties output to it
respondPalindrome :: String -> String
respondPalindrome = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not palindrome") . lines

main = do
  interact respondPalindrome
