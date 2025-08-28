--HC14T1: Initialize a Cabal Project
--Create a Haskell project using cabal init. Add a main executable that outputs "Hello, Cabal!".

module Main where

main :: IO ()
main = putStrLn "Hello, Cabal!"

--HC14T2: Add Dependency and Print Random Number
--Modify the .cabal file to include a dependency on the random package and print a random number between 1 and 100.

{-# LANGUAGE NumericUnderscores #-}

module Main where

main :: IO ()
main = do
  let bigNum = 1_000_000
  putStrLn $ "Big number: " ++ show bigNum



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

-- my-project.cabal
cabal-version:       3.4
name:                my-project
version:             0.1.0.0
build-type:          Simple

library
  hs-source-dirs:      src
  exposed-modules:     MyLibrary
  build-depends:       base >=4.17 && <5
  default-language:    Haskell2010

executable my-project-exe
  main-is:             Main.hs
  hs-source-dirs:      app
  build-depends:       base >=4.17 && <5,
                       my-project
  default-language:    Haskell2010


# cabal.project
packages: .


--src/MyLibrary.hs
module MyLibrary (greet) where

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"


--app/Main.hs
module Main where

import MyLibrary

main :: IO ()
main = putStrLn (greet "Matimba")




--HC14T8: Character Frequency Function
--Implement a function counts that returns a list of tuples showing character frequency in a string.

import Data.List (sort, group)

counts :: String -> [(Char, Int)]
counts str = map (\grp -> (head grp, length grp)) . group . sort $ str

main :: IO ()
main = do
    let text = "hello world"
    print $ counts text

--HC14T9: PartialTypeSignatures Extension
--Use the language extension PartialTypeSignatures to allow wildcard types in a function signature.

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

addNumbers :: _ -> _ -> Int
addNumbers x y = x + y

main :: IO ()
main = do
    print $ addNumbers 5 10
    print $ addNumbers 3 7


--HC14T10: Cabal Test Suite 
--Write a cabal test suite for a module that verifies correct behavior of the counts function.


--Counts.hs
module Counts (counts) where

-- Counts: (lines, words, characters)
counts :: String -> (Int, Int, Int)
counts input =
    let numLines = length (lines input)
        numWords = length (words input)
        numChars = length input
    in (numLines, numWords, numChars)



--CountsSpec.hs
module Main (main) where

import Test.Hspec
import Counts (counts)

main :: IO ()
main = hspec $ do
    describe "counts" $ do
        it "counts lines, words, and characters in a simple string" $ do
            counts "Hello world\nThis is Haskell" `shouldBe` (2, 5, 27)

        it "returns zeros for an empty string" $ do
            counts "" `shouldBe` (0, 0, 0)

        it "counts correctly with extra spaces" $ do
            counts "  Haskell   is   great " `shouldBe` (1, 3, 24)
			
			
--myproject.cabal
cabal-version:       2.4
name:                myproject
version:             0.1.0.0
build-type:          Simple

library
  hs-source-dirs:   src
  exposed-modules:  Counts
  build-depends:    base >=4.14 && <5
  default-language: Haskell2010

executable myproject
  main-is:          Main.hs
  hs-source-dirs:   app
  build-depends:    base, myproject
  default-language: Haskell2010

test-suite counts-test
  type:             exitcode-stdio-1.0
  main-is:          CountsSpec.hs
  hs-source-dirs:   test
  build-depends:    base, myproject, hspec >= 2.7
  default-language: Haskell2010





