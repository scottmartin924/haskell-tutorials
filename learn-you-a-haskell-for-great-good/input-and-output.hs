import Control.Monad

main = do
  putStrLn $ "What's your first name?"
  firstName <- getLine
  putStrLn $ "What's your last name?"
  lastName <- getLine
  putStrLn $ "Hey " ++ firstName ++ " " ++ lastName ++ ", how are you?"
  colors <- forM [1,2,3,4] (\a -> do
    putStrLn $ "Which color do you associate with " ++ show a ++ "?"
    -- NOTE: Could also declare variable here then explicitely return it, but this works as well
    getLine)
  putStrLn "The colors that you associate with 1,2,3,4 are: "
  mapM putStrLn colors
