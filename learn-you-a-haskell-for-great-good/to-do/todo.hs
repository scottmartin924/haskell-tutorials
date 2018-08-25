import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

-- Add to todo List
add :: [String] -> IO()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = badInput "add" 2

-- view todo list
view :: [String] -> IO()
view [fileName] = do
  contents <- readFile fileName
  let toDoTasks = lines contents
      numberedTasks = zipWith (\num line -> show num ++ " - " ++ line) [0..] toDoTasks
  putStrLn $ unlines numberedTasks
view _ = badInput "view" 1

-- Remove item from todo List
remove :: [String] -> IO()
remove [fileName, numberString] = do
  contents <- readFile fileName
  let toDoTasks = lines contents
      numberedTasks = zipWith (\num line -> show num ++ " - " ++ line) [0..] toDoTasks
  putStrLn "These are your TODO items:"
  mapM_ putStrLn numberedTasks
  putStrLn "Which one do you want to delete?"
  numberToDeleteStr <- getLine
  let numberToDelete = read numberToDeleteStr
      newToDoItems = unlines $ delete (toDoTasks !! numberToDelete) toDoTasks
  -- Ensure that if something goes wrong still cleaned up. First param is what to do if error
  -- second param is what to do it all goes well
  bracketOnError (openTempFile "." "temp")
    -- If error close tempfile handle and delete the temporary file
    (
      \(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName
    )
    (
      \(tempName, tempHandle) -> do
        hPutStr tempHandle newToDoItems
        hClose tempHandle
        removeFile fileName
        renameFile tempName fileName
    )
remove _ = badInput "remove" 2

-- IO action if command does not exist
doesNotExist :: String -> [String] -> IO()
doesNotExist command _ = putStrLn $ "The command " ++ command ++ " does not exist"

-- Bad input command message
badInput :: String -> Int -> IO()
badInput command numArgs = putStrLn $ "The command " ++ command ++ " takes " ++ show numArgs ++ " argument(s)"

-- Command to dispatch user input to correct program
-- First arg is command to run, second is arguments
dispatch :: String -> [String] -> IO()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch command = doesNotExist command

main = do
  (command:argsList) <- getArgs
  dispatch command argsList
