-- Example functions from ch5 - list comprehensions
import Data.Char

-- Take list of tuples and return list with second element of each tuple
seconds :: [(a,b)] -> [b]
seconds ts = [y | (_,y) <- ts]

-- find factors of a given number and return as list (this is not nearly optimized)
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- Check if number prime using factors (not very fast)
prime :: Int -> Bool
prime n = factors n == [1,n]

-- Custom implementation of zip
zip' :: [a] -> [b] -> [(a,b)]
zip' as bs = [(x,y) | x <- as, y <- bs]

-- turn letter into int (where 'a' is 0)
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- turn int into letter (where 'a' is 0)
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

-- Shift character by given shift facter (the first param is shift factor) (mod is for wrapping)
-- Only shift lowercase letters...leave all others the same
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

-- Encode message using shift function
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
