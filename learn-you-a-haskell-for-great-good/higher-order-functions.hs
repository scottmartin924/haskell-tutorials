-- Function that will create function for raising to any power
-- this will return a partially applied function which can then be used repeatedly
-- e.g. createPower 3 will return a function to cube a number then can call that function with a number to use it
createPower :: Int -> (Int -> Int)
createPower x y = y^x

-- Apply a function twice...just an example of taking a function as a parameter
-- first param is a function (a->a) second param is a value and function returns a value
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- custZipWith
-- Takes two lists and a function and zips list using the given function
custZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
custZipWith _ [] _ = []
custZipWith _ _ [] = []
custZipWith f (x:xs) (y:ys) = f x y : (zipWith f xs ys)

-- custom map function
custMap :: (a -> b) -> [a] -> [b]
custMap _ [] = []
custMap f (x:xs) = f x : (custMap f xs)

-- can do this better with fold...see below
custFilter :: (a -> Bool) -> [a] -> [a]
custFilter _ [] = []
custFilter f (x:xs)
    | f x = x : (custFilter f xs)
    | otherwise = custFilter f xs

firstCompleteWord :: String -> String
firstCompleteWord str = takeWhile (/= ' ') str

-- Create collatz chain for a given number
collatzChain :: Int -> [Int]
collatzChain 1 = [1]
collatzChain n
    | even n = n : collatzChain (n `div` 2)
    | odd n = n : collatzChain (3 * n + 1)

-- Sum list using foldl
custSum :: [Int] -> Int
-- custSum xs = foldl (\acc x -> acc + x) 0 xs
-- better
custSum xs = foldl (+) 0 xs

-- Find product of list using foldr (just to practice...no reason to use foldr over foldl here)
custProduct :: [Int] -> Int
-- Better yet...
-- custProduct xs = foldr (\x acc -> acc * x) 1 xs
custProduct xs = foldr (*) 1 xs

-- Reverse function using fold
-- NOTE: This seems confusing b/c we never reference the list that's passed in. What's happening
-- is we return the partially applied foldl function and then when the list is passed in to this
-- function (e.g. custReverse [1,2,3]) is completes the foldl function call which then executes
-- on that list. So we could've referenced it if we wanted to but don't need to
custReverse :: [a] -> [a]
custReverse = foldl (\acc x -> x : acc) []

-- Custom filter function using fold
custFoldFilter :: (a -> Bool) -> [a] -> [a]
custFoldFilter f = foldr (\x acc -> if f x then x : acc else acc) []

main :: IO()
main = do
    print $ "hello world"
    print $ square 4
    print $ cube 3
    print $ applyTwice (*2) 5
    print $ (zipWith (+) [1,2,3] [4,5,6])
    print $ zipWith max [5,8,12] [1,7,0]
    print $ custMap (*2) [1,3,5,7]
    print $ custFilter even [1..10]
    print $ firstCompleteWord "hello world this is me"
    print $ collatzChain 12
    -- filter with lambda function example
    print $ custFilter (\a -> a > 20) [1,34,23,0,10,19,20,112]
    -- Same function using partial function...no lambda required
    print $ custFilter (>20) [1,34,23,0,10,19,20,112]
    print $ custFoldFilter (>20) [1,34,23,0,10,19,20,112]
    print $ custSum [1..10]
    print $ custSum []
    print $ custProduct [1..10]
    print $ custProduct []
    print $ custReverse [1..10]
    print $ custReverse "hello world"
    -- function composition example
    print $ (square . cube) 4
    where
        cube = createPower 3
        square = createPower 2