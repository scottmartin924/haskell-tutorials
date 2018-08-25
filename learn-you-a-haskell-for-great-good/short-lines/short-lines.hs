-- Remove all lines in string with length > 10
-- NOTE: Lines splits string into array using newline character as separator
-- NOTE: unlines takes array and puts into one string using newline character as separator
shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines

main = do
  -- Could just use interact instead
  -- contents <- getContents
  -- putStr $ shortLinesOnly contents
  interact shortLinesOnly
