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

-- Produce a list with n identical elements
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- Select the nth element of a list
(!!^) :: [a] -> Int -> a
(x:_) !!^ 0 = x
(_:xs) !!^ n | n < 0 || n > length xs = error "Array index out of bounds"
             | otherwise = xs !!^ (n-1)

-- Decide if element is in list (this relies on short-circuiting the || operator to not look at entire list)
elem' :: Eq a => a -> [a] -> Bool
elem' val [] = False
elem' val (x:xs) = x == val || elem' val xs

-- #7 Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges
-- two sorted lists to give a single sorted list
  -- Future note: @ allows you to reference entire list (xs/ys) and still look at
  --head and tail as well (x:xt)/(y:yt) very useful here when need to use full list in next call
    -- Thanks LYAH
merge' :: Ord a => [a] -> [a] -> [a]
merge' [] ys = ys
merge' xs [] = xs
merge' xs@(x:xt) ys@(y:yt) | x < y = x : merge' xt ys
                           | otherwise = y : merge' xs yt

-- #8 Using merge define a function msort :: ord a => [a] -> [a] that implements
-- merge sort in which the empty list and singleton list are already sorted
-- and any other list is sorted by merging the two lists that result from sorting the
-- two lists separately...implementing halve w/ split at is probably a bit of cheating
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge' (mergeSort $ fst halved) (mergeSort $ snd halved)
                where halved = splitAt (length xs `div` 2) xs

-- #9 Construct library functions that do the following: sum list of numbers,
-- take a given number of elements from the start of a list, select last element
-- of a nonempty list
-- Sum list of numbers
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Take a given number of elements from the start of the list
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- Select the last element of a nonempty list
  -- Remember that to check for singleton list do [x] NOT (x:[])...just easier to read
last' :: [a] -> a
last' [] = error "cannot take last element of empty list"
last' [x] = x
last' (_:xs) = last' xs
