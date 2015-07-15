import System.Environment
import System.IO

import qualified Data.Map as Map
import qualified Data.List as List

-- To simulate the example format in Readme
getSeparator :: String -> String
getSeparator word
  | len < sampleLen = replicate (sampleLen - len) ' '
  | otherwise = replicate longWordLen ' '
  where len = length word
        sampleLen = 28
        longWordLen = 4

countWords :: [String] -> [(String, Int)]
countWords = Map.toList . List.foldl' insertWord Map.empty
  where 
    insertWord :: Map.Map String Int -> String -> Map.Map String Int
    insertWord accMap word = Map.insertWith (+) word 1 accMap

getOutput :: [(String, Int)] -> String
getOutput = unlines . map showEntry
  where
    showEntry :: (String, Int) -> String
    showEntry (word, count) = word ++ getSeparator word ++ show count

main = do
  [inputPath, outputPath] <- getArgs
  input <- readFile inputPath
  writeFile outputPath $ getOutput $ countWords $ words input
