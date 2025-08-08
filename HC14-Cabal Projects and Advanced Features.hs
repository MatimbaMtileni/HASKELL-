--HC14T1: Initialize a Cabal Project
--Create a Haskell project using cabal init. Add a main executable that outputs "Hello, Cabal!".

module Main where

main :: IO ()
main = putStrLn "Hello, Cabal!"

--HC14T2: Add Dependency and Print Random Number
--Modify the .cabal file to include a dependency on the random package and print a random number between 1 and 100.



--HC14T3: NumericUnderscores Extension
--Enable the NumericUnderscores extension. Create variables with large numbers and print them.

{-# LANGUAGE NumericUnderscores #-}

module Main where

userGems :: Integer
userGems = 15_894_231

purchase :: Integer -> Integer -> Integer
purchase gems item = gems - item

level1Vest :: Integer
level1Vest = 52_000

level2Vest :: Integer
level2Vest = 147_000

level3Vest :: Integer
level3Vest = 845_000

tank :: Integer
tank = 314_159_265_358

main :: IO ()
main = do
  let remaining = purchase userGems level2Vest
  putStrLn $ "User gems after purchase: " ++ show remaining
  putStrLn $ "Tank value: " ++ show tank

--HC14T4: TypeApplications Extension
--Enable the TypeApplications extension and create a function that reads a String and converts it to an Int using read.

{-# LANGUAGE TypeApplications #-}

module Main where

main :: IO ()
main = do
  let strNum = "123"
      intVal = read @Int strNum
  print intVal

--HC14T5: Custom Data Type and Pattern Matching with @
--Write a Haskell program that uses a custom data type Result a and demonstrate pattern matching using the @ symbol.


module Main where

data Result a = Success a | Failure String deriving Show

handleResult :: Result Int -> String
handleResult r@(Success val) = "Got success: " ++ show val ++ " from " ++ show r
handleResult r@(Failure msg) = "Failure with: " ++ msg ++ " from " ++ show r

main :: IO ()
main = do
  print $ handleResult (Success 42)
  print $ handleResult (Failure"hard luck" )

--HC14T6: Project Structure: src and app
--Create a cabal project structure with src and app directories. Place the main module in the app folder and additional modules in src.

module Main where

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

main :: IO ()
main = putStrLn $ greet "haskell class"

--HC14T7: Library Component in Cabal
--Modify the .cabal file to support a library component alongside the main executable.



--HC14T8: Character Frequency Function
--Implement a function counts that returns a list of tuples showing character frequency in a string.



--HC14T9: PartialTypeSignatures Extension
--Use the language extension PartialTypeSignatures to allow wildcard types in a function signature.



--HC14T10: Cabal Test Suite
--Write a cabal test suite for a module that verifies correct behavior of the counts function.