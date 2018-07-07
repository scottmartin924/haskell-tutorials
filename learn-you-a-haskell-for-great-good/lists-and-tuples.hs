-- List comprehension examples
-- Remove everything except capital letters
stripLower string = [x | x <- string, x `elem` ['A'..'Z']]

-- Length of list
customLength list = sum [1 | _ <- list]

-- Find pythagorean triples up to max side length max...this is probably very slow
-- but I'm just trying to use a list comprehension for something
pythTriples max = [(a,b,c) | c <- [1..max], b <- [1..c], a <- [1..b],
                    a^2+b^2==c^2]

main = do
    let list = [1,2,3,4,5]
    print list
    -- concat list
    print $ list ++ [7,8,9]
    -- prepend list (using cons operator)
    print $ 0:list
    -- Get nth element of list
    print $ list !! 3
    -- Get first element of list
    print $ head list
    -- Get last element of list
    print $ last list
    -- Return list containing all but first elemtn
    print $ tail list
    -- Return list containing all but last elemnt
    print $ init list
    -- check if 1 in list (using infix notation then regular)
    print $ 3 `elem` list
    -- print $ elem 3 list
    -- basic list comprehension to add 2
    print $ [x+2 | x <- list]
    -- Only get even numbers (and add 2 to them)
    print $ [x+2 | x <- list, even x]
    print $ customLength [1..20]
    print $ stripLower "HELLO, Scott"
    -- find pythagorean triples wiht side length <=30
    print $ pythTriples 30