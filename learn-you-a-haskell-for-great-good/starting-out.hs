-- Basic function to square value
square x = x^2

squareIfSmallerThan x y = if x < y
                    then square x
                    else x

main = do
    putStr "50 * 100 = "
    print $ 50*100
    putStr "5 equals 5"
    print $ 5==5
    putStr "5 does not equal 5"
    print $ 5/=5
    -- This would fail b/c different types
    -- 5/="hello"
    -- Proof that fuction evaluation has highest precedence...should get 100 (not 91)
    print $ succ 9*10
    -- Infix function call
    print $ 9 `div` 3
    putStr "2^2"
    print $ square 2
    putStr "5^2"
    print $ square 5
    putStr "2.4^2"
    print $ square 2.4
    putStr "sqareIfsmallerThan 4 5"
    print $ squareIfSmallerThan 4 5
    putStr "squareifSmallerThan 5 4"
    print $ squareIfSmallerThan 5 4