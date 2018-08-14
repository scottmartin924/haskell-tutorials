main :: IO()
main = do
  print $ (map cracklePop [1..100])
  where cracklePop x
            | x `mod` 15 == 0 = "CracklePop"
            | x `mod` 5 == 0 = "Crackle"
            | x `mod` 3 == 0 = "Pop"
            | otherwise = show x
