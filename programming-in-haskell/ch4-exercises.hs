-- Function to split list in half
halve :: [a] -> ([a],[a])
halve list = splitAt (length list `div` 2) list

-- third is function to get third element of list...defined below in 3 different ways
-- NOTE: Excercise has you assume that there are 3 elements so no checking here
-- Third defined with head/tail
thirdHt :: [a] -> a
thirdHt = head . tail . tail

-- third defined w/ list indexing
thirdIdx :: [a] -> a
thirdIdx xs = xs !! 2

-- third deined with a pattern
thirdPattern :: [a] -> a
thirdPattern (_:_:x:_) = x

-- Safetail should act like tail but return empty list for passed in empty list.
-- Implemented in 3 ways below
-- safetail with conditional
safetailCond :: [a] -> [a]
safetailCond xs = if null xs then [] else tail xs

-- safetail with guards
safetailGuards :: [a] -> [a]
safetailGuards xs | null xs = []
                  | otherwise = tail xs

-- safetail with pattern matching
safetailPattern :: [a] -> [a]
safetailPattern [] = []
safetailPattern (_:xs) = xs

-- Showing identical 3 value multiplication using "normal" curried functions and a lambda
mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

multLambda :: Int -> (Int -> (Int -> Int))
multLambda = \x -> (\y -> (\z -> x*y*z))

luhnDouble :: Int -> Int
luhnDouble x | (*2) x > 9 = (*2) x - 9
             | otherwise = (*2) x
             -- Nicer answer online  use y = x*2 and then return y - (if y > 9 then 9 else 0)...I like that

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = doublesSum `mod` 10 == 0
                  where doublesSum = sum [luhnDouble a, b, luhnDouble c, d]
