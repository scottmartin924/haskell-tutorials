-- examples from ch7 - higher order functions
-- higher order function = function that takes a function as an argument

-- Custom map function using recursion (which applies only to lists...so actually less useful)
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- Custom filter function using recursion (again applies only to lists...moreover
-- list comprehension is really a cleaner way to do this)
filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) | f x = x : filter' f xs
                 | otherwise = filter' f xs

-- Determine if all elements of list satisfies function
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' f (x:xs) = f x && all' f xs

-- Determine if any elemnt of list satisfies function
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs) = f x || any' f xs

-- Take elements from list until condition is false
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) | f x = x : takeWhile' f xs
                    | otherwise = []

-- Drop elements from list until condition is false
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f xs@(x:xt) | f x = dropWhile' f xt
                    | otherwise = xs

-- List length defn using fold
lengthFold :: Num a => [a] -> Int
lengthFold = foldr (\_ n -> n+1) 0
