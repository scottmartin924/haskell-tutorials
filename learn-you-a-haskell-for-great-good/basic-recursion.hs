-- Basic max function implemented recursively
-- Reminder: => is about type so ned ord a => then use -> for params and return
recMax :: (Ord a) => [a] -> a
recMax [] = error "No max in empty list"
recMax [x] = x
recMax (x:xs) = max x (recMax xs)

-- Fibonacci recursion
-- Need the 0 one for if someone passes in 0
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib(x-2)

-- Basic zip function
custZip :: [a] -> [b] -> [(a,b)]
custZip _ [] = []
custZip [] _ = []
custZip (x:xs) (y:ys) = (x,y) : custZip xs ys

-- Quicksort (I hope)
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerEq = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quicksort smallerEq ++ [x] ++ quicksort larger

main :: IO()
main = do
    print $ recMax [3,2,64,7,10,32]
    print $ fib 10
    print $ custZip [1,2,3] ['a','b','c']
    print $ quicksort [8,1,3,9,2,10,1]
    print $ quicksort "recursion is cool"