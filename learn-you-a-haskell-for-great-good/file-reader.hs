import System.IO
import Data.Char

main = do
  content <- readFile file
  -- Take file and make uppercase version of it
  writeFile dest (map toUpper content)
  where file = "never-quite-free.txt"
        dest = "new-file.txt"
