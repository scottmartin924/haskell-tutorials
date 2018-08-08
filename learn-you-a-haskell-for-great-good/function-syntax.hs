import           BasicModule
import           Data.List

-- basic pattern matching
lucky:: Int -> String
lucky 7 = "wahhooo... 7 rules"
lucky x = "better luck next time"

-- pattern matching factorial
-- Didn't put in check to ensure within in value range
-- notice here that if don't put parens around x-1 this will recurse infinitely
factorial :: Int -> Int
factorial 0 = 1
factorial x = x*factorial (x-1)

-- Add 3d vectors pattern matching
vectorAdd :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
vectorAdd (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

-- List pattern matching
-- MUST have () around x:_ b/c we're binding to 2 variables
-- The (x:_) syntax to get first element of list only works for non-empty list so must have pattern for []
rsmHead :: [a] -> a
rsmHead []    = error "No head on empty list"
rsmHead (x:_) = x

-- Example with as expression
exampleListFnct :: String -> String
exampleListFnct [] = "empty list"
exampleListFnct fullList@(x:xs) = "First element of list " ++ fullList ++ " is " ++ [x] ++ ", rest is " ++ xs

-- Very basic where block example using guards (pattern matching would've worked too? Just where loses scope)
gradeAverage :: [Int] -> Char
gradeAverage grades
    | average < 60 = 'F'
    | average < 70 = 'D'
    | average < 80 = 'C'
    | average < 90 = 'B'
    | otherwise    = 'A'
    where average = realToFrac (sum grades)/(genericLength grades)


main :: IO()
main = do
    print $ "Lucky 7"
    print $ lucky 7
    print $ "Lucky 2"
    print $ lucky 2
    print $ factorial 8
    print $ vectorAdd (1,2,3) (4,5,6)
    print $ rsmHead exampleList
    print $ exampleListFnct "hello world"
    print $ gradeAverage [20,60,80]
    print $ gradeAverage [100,50, 75]
    print $ sumOfSquares 2 4
    print $ sumOfCubes 2 4
    where exampleList = [2,5,8,1,3]
