--HC12T1: Print a Welcome Message
--Create a Haskell program that prints "Welcome to Haskell Programming!" to the terminal.

main :: IO ()
main = putStrLn "Welcome to Haskell Programming!"

--HC12T2: Add Two Numbers
--Write a function addTwoNumbers that takes two integers as input and returns their sum. The result should be printed using the main function.

addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

main :: IO ()
main = do
  putStrLn "Enter first number:"
  x <- readLn
  putStrLn "Enter second number:"
  y <- readLn
  putStrLn $ "The sum is: " ++ show (addTwoNumbers x y)  


--HC12T3: Factorial Function
--Create a program that defines a function factorial to compute the factorial of a given positive integer.

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
  putStrLn "Enter a number:"
  n <- readLn
  putStrLn $ "Factorial is: " ++ show (factorial n) 


--HC12T4: First 10 Fibonacci Numbers
--Write a program that calculates and prints the first 10 Fibonacci numbers using recursion.

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

main :: IO ()
main = print $ map fibonacci [0..9] 

--HC12T5: Palindrome Checker
--Create a program that defines a function isPalindrome to check whether a given string is a palindrome. Use the main function to test this with user input.

isPalindrome :: String -> Bool
isPalindrome str = cleaned == reverse cleaned
  where cleaned = map toLower (filter (/= ' ') str)

import Data.Char (toLower)

main :: IO ()
main = do
  putStrLn "Enter a string:"
  input <- getLine
  if isPalindrome input
    then putStrLn "It is a palindrome."
    else putStrLn "It is not a palindrome."


--HC12T6: Sort a List of Integers
--Develop a program that reads a list of integers from the user and prints the list sorted in ascending order.

import Data.List (sort)

main :: IO ()
main = do
  putStrLn "Enter a list of integers separated by spaces:"
  input <- getLine
  let nums = map read (words input) :: [Int]
  putStrLn $ "Sorted list: " ++ show (sort nums)

--HC12T7: Calculate Circle Area
--Create a function calculateCircleArea that calculates the area of a circle given its radius. The main function should demonstrate its usage.


calculateCircleArea :: Float -> Float
calculateCircleArea r = pi * r * r

main :: IO ()
main = do
  putStrLn "Enter the radius:"
  r <- readLn
  putStrLn $ "Area of the circle is: " ++ show (calculateCircleArea r)



--HC12T8: Merge Two Sorted Lists
--Define a function mergeLists that merges two sorted lists into a single sorted list. Use the main function to test this functionality.

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
  | x < y     = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

main :: IO ()
main = do
  putStrLn "Enter first sorted list:"
  list1 <- fmap (map read . words) getLine
  putStrLn "Enter second sorted list:"
  list2 <- fmap (map read . words) getLine
  let merged = mergeLists list1 list2
  putStrLn $ "Merged list: " ++ show merged


--HC12T9: Read and Print File Content
--Write a program that reads a file and prints its content to the terminal. Handle errors gracefully in case the file doesn't exist.

import System.IO
import Control.Exception

main :: IO ()
main = do
  putStrLn "Enter filename:"
  fileName <- getLine
  result <- try (readFile fileName) :: IO (Either IOError String)
  case result of
    Left _  -> putStrLn "Error: File does not exist or could not be read."
    Right content -> putStrLn content


--HC12T10: Mathematical Operations Module
--Create a Haskell program that defines a module for mathematical operations and demonstrates its use in the main function.


module MathOps (add, subtract', multiply, divide) where

add :: Int -> Int -> Int
add x y = x + y

subtract' :: Int -> Int -> Int
subtract' x y = x - y

multiply :: Int -> Int -> Int
multiply x y = x * y

divide :: Int -> Int -> Maybe Float
divide _ 0 = Nothing
divide x y = Just (fromIntegral x / fromIntegral y)


import MathOps

main :: IO ()
main = do
  let x = 10
  let y = 5
  putStrLn $ "Add: " ++ show (add x y)
  putStrLn $ "Subtract: " ++ show (subtract' x y)
  putStrLn $ "Multiply: " ++ show (multiply x y)
  case divide x y of
    Just result -> putStrLn $ "Divide: " ++ show result
    Nothing -> putStrLn "Cannot divide by zero."
