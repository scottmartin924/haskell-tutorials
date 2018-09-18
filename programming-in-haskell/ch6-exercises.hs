-- #1 Modify definition of factorial definition
-- to prohibit negative arguments by adding a guard
-- to the recursive case
factorial' :: Int -> Int
factorial' 0 = 1
factorial' x | x < 0 = 0
             | otherwise = x * factorial' (x-1)

-- #2 Define a recursive function sumdown :: Int -> Int that
-- returns the sum of the non-negative integers from a given
-- value down to zero. (factorial for addition)
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n < 0 = 0
          | otherwise = n + sumdown (n-1)

-- #3 Define the exponentiation operator ^ for non-negative
-- integers using recursion
(^!) :: Int -> Int -> Int
base ^! 0 = 1
base ^! ex = base * (base ^! (ex-1))

-- #4 Define a recusurive function euclide :: Int -> Int -> that implements
-- Euclid's algorithm for calculating the greatest common divisor of two
-- non-negative numbers
euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x < y = euclid x (y-x)
           | otherwise = euclid (x-y) y

-- #5 Using the recursion implment the list functions length, drop, and init
-- return length of list
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- Return list after dropping first n vals
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (_:xs) = drop' (n-1) xs

-- Return last element of nonempty list
init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- #6 Define the following library functions using recursion
-- Decide if all logical values in a list are True
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

-- Concat a list of lists
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs
