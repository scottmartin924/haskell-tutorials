import Data.Char
import Control.Applicative

main = do
  -- line <- fmap (reverse . map toUpper) getLine
  -- putStrLn $ "You said " ++ line ++ " backwards"
  -- Turns out fmap for functions is same as function composition
  print $ (+3) . (*2) $ 3
  print $ fmap (+3) (*2) $ 3
  -- Below are some basic examples of applicative style functors...could have
  -- just done in ghci but wanted to remember them
  -- NOTE: Applicative functors are left associative
  print $ pure (*) <*> Just 3 <*> Just 5
  print $ pure (+) <*> Nothing <*> Just 5
  print $ pure (**) <*> Just 2 <*> Just 5
  print $ (++) <$> Just "Scott" <*> Just " was here"
  print $ [(*3),(+2)] <*> [1,3]
  print $ [(*),(+)] <*> [1,2] <*> [3,4]
  -- This will give every possible combo of multiplication across the two lists
  print $ (*)<$>[1,2,3]<*>[4,5,6]
  -- cartesian product of list...(,) is same as (x,y)
  print $ (,) <$> [1,2,3] <*> [4,5,6]
