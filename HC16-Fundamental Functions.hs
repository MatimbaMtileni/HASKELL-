--HC16T1: Reverse a String
--Define a function to reverse a string.

reverseString :: String -> String
reverseString = reverse

main :: IO ()
main = do
    putStrLn "Enter a string to reverse:"
    input <- getLine
    let reversed = reverseString input
    putStrLn $ "Reversed string: " ++ reversed


--HC16T2: Palindrome Checker
--Define a function that checks if a given string is a palindrome.

isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

main :: IO ()
main = do
    putStrLn "Enter a string to check if it's a palindrome:"
    input <- getLine
    if isPalindrome input
        then putStrLn "Yes, It's a palindrome."
        else putStrLn "No, It's not a palindrome."

--HC16T3: Factorial
--Define a function to calculate the factorial of a number.

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
    putStrLn "Enter a non-negative integer to calculate its factorial:"
    input <- getLine
    let n = read input :: Integer
    if n < 0
        then putStrLn "Factorial is not defined for negative numbers."
        else putStrLn $ "Factorial of " ++ show n ++ " is " ++ show (factorial n)

--HC16T4: Filter Even Numbers
--Implement a function that filters only even numbers from a list.

filterEvens :: [Int] -> [Int]
filterEvens = filter even

main :: IO ()
main = do
    putStrLn "Enter a list of integers:"
    input <- getLine
    let numbers = map read (words input) :: [Int]
    let evens = filterEvens numbers
    putStrLn $ "Even numbers: " ++ show evens

--HC16T5: Uppercase String
--Define a function to convert all characters in a string to uppercase.

import Data.Char (toUpper)

toUpperCase :: String -> String
toUpperCase = map toUpper

main :: IO ()
main = do
    putStrLn "Enter a string:"
    input <- getLine
    let upper = toUpperCase input
    putStrLn $ "Uppercase string: " ++ upper

--HC16T6: nth Fibonacci Number
--Implement a function that returns the nth Fibonacci number.

fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main :: IO ()
main = do
    putStrLn "Enter a integer:"
    input <- getLine
    let n = read input :: Int
    if n < 0
        then putStrLn "Please enter integer."
        else putStrLn $ "Fibonacci number " ++ show n ++ " is " ++ show (fibonacci n)

--HC16T7: Element Existence in List
--Write a function that checks if an element exists in a list.

elementExists :: Eq a => a -> [a] -> Bool
elementExists = elem

main :: IO ()
main = do
    putStrLn "Enter a list of elements:"
    input <- getLine
    let list = words input  
    putStrLn "Enter an element to check for existence:"
    element <- getLine
    if elementExists element list
        then putStrLn $ "Yes! \"" ++ element ++ "\" exists in the list."
        else putStrLn $ "No! \"" ++ element ++ "\" does not exist in the list."


--HC16T8: Insertion Sort
--Define a function that sorts a list of integers using the insertion sort algorithm.

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y : z : zs
      | otherwise = z : insert y zs

main :: IO ()
main = do
    putStrLn "Enter a list of integers:"
    input <- getLine
    let numbers = map read (words input) :: [Int]
    let sorted = insertionSort numbers
    putStrLn $ "Sorted list: " ++ show sorted

--HC16T9: Remove Duplicates from List
--Implement a function to remove duplicates from a list.

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

main :: IO ()
main = do
    putStrLn "Enter a list of elements"
    input <- getLine
    let list = words input  
    let uniqueList = removeDuplicates list
    putStrLn $ "List after removing duplicates: " ++ show uniqueList

--HC16T10: Character Frequency in String
--Define a function that counts the frequency of each character in a string.

import Data.Map (Map)
import qualified Data.Map as Map

charFrequency :: String -> Map Char Int
charFrequency = Map.fromListWith (+) . map (\c -> (c, 1))

main :: IO ()
main = do
    putStrLn "Enter a string to count character frequencies:"
    input <- getLine
    let freqMap = charFrequency input
    putStrLn "Character frequencies:"
    mapM_ (\(c, count) -> putStrLn $ show c ++ ": " ++ show count) (Map.toList freqMap)