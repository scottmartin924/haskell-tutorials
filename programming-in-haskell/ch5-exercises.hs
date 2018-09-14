-- Ch5 - List comprehensions exercises

-- Using a list comprehension give an expression that calculates the sum
-- 1^2+2^2+...+100^2 of the first one hundred integer squares
sumSquares :: Int -> Int -> Int
sumSquares start end = sum [x^2 | x <- [start..end]]

-- Suppose that a coordinate grid of size m x n is given by the list of all pairs
-- (x,y) of integers s.t. 0<=x<=m and 0<=y<=n. Using a list comprehension define
-- a function grid :: Int -> Int -> [(Int, Int)] that returns a coordinate grid
-- of a given size
grid :: Int -> Int -> [(Int, Int)]
grid l w = [(x,y) | x <- [0..l], y <- [0..w]]

-- Using a list comprehension and the function grid above define a function
-- square :: Int -> [(Int, Int)] that returns a coordinate square of size n,
-- excluding the diagonal from (0,0) to (n,n)
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- In a similar way to the function length show how the library function
-- replicate :: Int -> a -> [a] that produces a list of identical elements can
-- be defined using a list comprehension
replicate' :: Int -> a -> [a]
replicate' num val = [val | _ <- [1..num]]

-- A triple (x,y,z) of positive integers is Pythagorean if it satisfies the equation
-- x^2+y^2=z^2. Using a list comprehension with three generators define a function
-- pyths :: Int -> [(Int, Int, Int)] that returns the list of all such triples
-- whose components are at most a given limit
pyths :: Int -> [(Int, Int, Int)]
pyths maxVal = [(x,y,z) | z <- [1..maxVal], x <- [1..z], y <- [1..x], x^2 + y^2 == z^2]

-- A positive integer is perfect if it equals the sum of all of its factors excluding
-- the number itself. Using a list comprehension and the function factors define
-- a function perfects :: Int -> [Int] that returns the list of all perfect
-- numbers up to a given limit
perfect :: Int -> Bool
perfect n = sum [x | x <- [1..n], n `mod` x == 0, n /= x] == n

perfects :: Int -> [Int]
perfects maxVal = [x | x <- [1..maxVal], perfect x]

-- Redefine the function positions using the function find
positions' :: Eq a => a -> [a] -> [Int]
positions' val list = find val (zip list [0..])

-- find function to be used in next question...finds the key values from the tuples
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k==k']

-- The scalar product of two lists of integers xs and ys of length n is given by
-- the sum of the products of corresponding integers. In a similar manner to chisqr
-- show how a list comprehension can be used to define a function
-- scalarproduct :: [Int] -> [Int] -> Int that returns the scalar product of two lists
-- NOTE: Remember that if you're looping over two lists together just zip them
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]
