
-- implement postfix (I think that's right?) calculator
-- Pretty fragile for bad input
postfixCalc :: String -> Double
postfixCalc = head . foldl foldingFnc [] . words
  where foldingFnc (x:y:ys) "*" = (y*x):ys
        foldingFnc (x:y:ys) "+" = (y+x):ys
        foldingFnc (x:y:ys) "-" = (y-x):ys
        foldingFnc (x:y:ys) "^" = (y**x):ys
        foldingFnc (x:y:ys) "/" = (y/x):ys
        foldingFnc xs numberString = read numberString:xs

main = do
  print $ postfixCalc "1 2 + 3 * 2 - 8 / 2 ^"
