--HC13T1: List Files in Directory
--Create a program that lists all files in the current directory using the System.Directory module.

import System.Directory (listDirectory)

main :: IO ()
main = do
  files <- listDirectory "."
  putStrLn "Files in current directory:"
  mapM_ putStrLn files


--HC13T2: Filter Files by Substring
--Write a function that filters files in the current directory based on a substring in the filename using Data.List.isInfixOf.

import System.Directory (listDirectory)
import Data.List (isInfixOf)

filterFiles :: String -> IO [FilePath]
filterFiles keyword = do
  files <- listDirectory "."
  return $ filter (isInfixOf keyword) files

main :: IO ()
main = do
  let keyword = "Creating"  
  matches <- filterFiles keyword
  putStrLn $ "Filtered files with \"" ++ keyword ++ "\":"
  mapM_ putStrLn matches



--HC13T3: Sort and Return Filtered Files
--Implement a function that sorts and returns filtered file names from the current directory using both Data.List.sort and Data.List.filter.

import System.Directory (listDirectory)
import Data.List (isInfixOf, sort)

sortFilteredFiles :: String -> IO [FilePath]
sortFilteredFiles keyword = do
  files <- listDirectory "."
  return $ sort $ filter (isInfixOf keyword) files

main :: IO ()
main = do
  let keyword = "Creating"
  sortedMatches <- sortFilteredFiles keyword
  putStrLn $ "Sorted and filtered files with \"" ++ keyword ++ "\":"
  mapM_ putStrLn sortedMatches

--HC13T4: SumNonEmpty Module
--Write a Haskell module named SumNonEmpty that defines a function sumNonEmpty, which returns an error if called on an empty list.

module SumNonEmpty where

data MyData a b = Error a | Result b deriving (Show)

sumNonEmpty :: Num a => [a] -> MyData String a
sumNonEmpty [] = Error "List is empty"
sumNonEmpty xs = Result (sum xs)

--HC13T5: Restrict Module Export List
--Refactor the sumNonEmpty function to restrict the visibility of helper functions like error messages in the module export list.


module SumNonEmpty2 (sumNonEmpty2, MyData2(..)) where

errorMessage :: String
errorMessage = "List is empty"

data MyData2 a b = Error2 a | Result2 b deriving (Show)

sumNonEmpty2 :: Num a => [a] -> MyData2 String a
sumNonEmpty2 [] = Error2 errorMessage
sumNonEmpty2 xs = Result2 (sum xs)

--HC13T6: File Names to Map
--Create a function that uses Data.Map to convert a list of filtered file names into a key-value map.

import qualified Data.Map as Map

fileListToMap :: [FilePath] -> Map.Map Int FilePath
fileListToMap files = Map.fromList $ zip [1..] files

main :: IO ()
main = do
  let sampleFiles = ["file1.txt", "file2.txt", "file3.txt"]
  print $ fileListToMap sampleFiles


--HC13T7: Use Custom Module in Main
--Write a program that imports a function from your custom SumNonEmpty module and calculates the sum of a list of numbers.

sumNonEmpty2 :: Num a => [a] -> a
sumNonEmpty2 [] = 0
sumNonEmpty2 xs = sum xs

main :: IO ()
main = do
  let result = sumNonEmpty2 [10, 20, 30]
  print result

--HC13T8: Qualified Imports for Name Conflicts
--Demonstrate how to handle name conflicts between two imported modules, using qualified imports.

import qualified Data.List as List
import qualified Data.Map as Map

main :: IO ()
main = do
  let names = ["zulu", "Tsonga", "Sotho"]
  let sorted = List.sort names
  let mapped = Map.fromList $ zip [1..] sorted
  print mapped


import qualified Data.List as List
import qualified Data.Map as Map

main :: IO ()
main = do
  let names = ["zulu", "Tsonga", "Sotho"]
  let sorted = List.sort names
  let mapped = Map.fromList $ zip [1..] sorted
  print mapped


--HC13T9: Renaming Module Namespace
--Create a function that demonstrates renaming a module namespace and uses functions from both renamed modules.

import qualified Data.List as L
import qualified Data.Map as M

toMapSorted :: [String] -> M.Map Int String
toMapSorted names = M.fromList $ zip [1..] (L.sort names)

main :: IO ()
main = do
  let result = toMapSorted ["banana", "apple", "cherry"]
  print result


--HC13T10: Multi-Module Main Function
--Create a main function that imports and calls functions from at least two modules (System.Directory, Data.List) to perform a search and display sorted file results.

import System.Directory (listDirectory)
import Data.List (isInfixOf, sort)

main :: IO ()
main = do
  let keyword = "Creating"
  files <- listDirectory "."
  let results = sort $ filter (isInfixOf keyword) files
  putStrLn $ "Sorted matching files with \"" ++ keyword ++ "\":"
  mapM_ putStrLn results
