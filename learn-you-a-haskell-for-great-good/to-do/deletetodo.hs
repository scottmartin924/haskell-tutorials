import System.IO
import System.Directory
import Data.List
import Control.Exception -- Needed for brackets function

main = do
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
  where fileName = "todo.txt"
