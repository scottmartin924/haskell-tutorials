-- Find product of list
custProduct :: Num a => [a] -> a
custProduct [] = 1
custProduct (x:xs) = x * custProduct xs

-- Quicksort for list
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
                    where
                      smaller = [a | a <- xs, a <=x]
                      larger = [b | b <- xs, b > x]

-- Quicksort unique
quickSortUnique :: Ord a => [a] -> [a]
quickSortUnique [] = []
quickSortUnique (x:xs) = quickSortUnique smaller ++ [x] ++ quickSortUnique larger
                    where
                      smaller = [a | a <- xs, a < x]
                      larger = [b | b <- xs, b > x]

-- Quicksort reverse
quickSortReverse :: Ord a => [a] -> [a]
quickSortReverse [] = []
quickSortReverse (x:xs) = quickSortReverse larger ++ [x] ++ quickSortReverse smaller
                    where
                      smaller = [a | a <- xs, a <= x]
                      larger = [b | b <- xs, b > x]

main = do
  print $ product [1..5]
  print $ product [2..4]
  print $ quickSort val
  print $ quickSortUnique val
  print $ quickSortReverse val
    where
      val = [5,9,2,1,4,0,1,11,2]
