-- Ch5 - List comprehensions exercises

-- Function to calculate sum of squares of sequence of numbers. Inputs are start and end of sequence
sumSquares :: Int -> Int -> Int
sumSquares start end = sum [x^2 | x <- [start..end]]

-- create coordinate grid of given size (first param is length, second width in units)
grid :: Int -> Int -> [(Int, Int)]
grid l w = [(x,y) | x <- [0..l], y <- [0..w]]

-- Return square coordinate grid of given size excluding the diagonal
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- Use list comprehension to create replicate
replicate' :: Int -> a -> [a]
replicate' num val = [val | _ <- [1..num]]

-- find list of all pythagorean triples up to given value
pyths :: Int -> [(Int, Int, Int)]
pyths maxVal = [(x,y,z) | z <- [1..maxVal], x <- [1..z], y <- [1..x], x^2 + y^2 == z^2]

-- Check if number is perfect
perfect :: Int -> Bool
perfect n = sum [x | x <- [1..n], n `mod` x == 0, n /= x] == n

-- Find all perfect (if equals sum of all factors excluding itself) numbers up to max val
perfects :: Int -> [Int]
perfects maxVal = [x | x <- [1..maxVal], perfect x]
