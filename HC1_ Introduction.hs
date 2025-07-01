HC1T1 - Task 1: Function Composition

Define three functions:

1. double: Multiplies a number by 2.
2. increment: Increases a number by 1.
3. doubleThenIncrement: Uses function composition to apply double first and then increment.

-- Define functions
double :: Num a => a -> a
double x = 2 * x

increment :: Num a => a -> a
increment x = x + 1

doubleThenIncrement :: Num a => a -> a
doubleThenIncrement = increment . double

-- Main function to test the above
main :: IO ()
main = do
    print (double 5)              
    print (increment 5)           
    print (doubleThenIncrement 5)
	

HC1T2 - Task 2: Pure Function Example
Write a function circleArea that calculates the area of a circle given the radius. Ensure that it’s pure and does not depend on any external state.

-- Define a pure function to calculate the area of a circle
circleArea :: Floating a => a -> a
circleArea r = pi * r * r

-- Main function to test circleArea
main :: IO ()
main = do
    let radius = 5.0
    let area = circleArea radius
    putStrLn ("The area of a circle with radius " ++ show radius ++ " is " ++ show area)
	
	
HC1T3 - Task 3: Checking if a Number is Greater than 18
Write a function greaterThan18 that checks whether a given number is greater than 18.

-- Define a function to check if a number is greater than 18
greaterThan18 :: (Ord a, Num a) => a -> String
greaterThan18 n =
    if n > 18
        then "it's greater than 18."
        else "it's not greater than 18."

-- Main function to test the above
main :: IO ()
main = do
    print (greaterThan18 20)   
    print (greaterThan18 15) 
	
HC1T4 - Task 4: Composing a Function to Process Player Data
Write three functions:

extractPlayers: Takes a list of tuples ((name, score)) and extracts the player names.
sortByScore: Sorts the list of players by score in descending order.
topThree: Returns the top three players.
Compose these functions into getTopThreePlayers.

import Data.List (sortBy)

-- 1. Extract player names
extractPlayers :: [(String, Int)] -> [String]
extractPlayers = map fst

-- 2. Sort players by score in descending order
sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = reverse . sortBy (\(_, s1) (_, s2) -> compare s1 s2)

-- 3. Get top three players
topThree :: [(String, Int)] -> [(String, Int)]
topThree = take 3

-- 4. Compose all into getTopThreePlayers
getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- 5. Main function to run the program
main :: IO ()
main = do
  let players = [("Alice", 10), ("Bob", 25), ("Charlie", 15), ("Dave", 20), ("Eve", 5)]
  let topPlayers = getTopThreePlayers players
  putStrLn "Top 3 Players:"
  mapM_ putStrLn topPlayers
  
 HC1T5 - Task 5: Laziness in Haskell
Create a function infiniteNumbers that generates an infinite list of numbers. Extract only the first n elements.

-- Generates an infinite list of natural numbers starting from 1
infiniteNumbers :: [Integer]
infiniteNumbers = [1..]

-- Takes the first n numbers from the infinite list
firstN :: Int -> [Integer]
firstN n = take n infiniteNumbers

-- Main function to test the above
main :: IO ()
main = do
    putStrLn "First 10 numbers from the infinite list:"
    print (firstN 10)
	

HC1T6 - Task 6: Using Type Signatures
Define a function addNumbers that takes two integers and returns their sum.

-- Function that adds two integers
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- Main function to test addNumbers
main :: IO ()
main = do
    putStrLn "\nSum of 5 and 7 using addNumbers:"
    print $ addNumbers 5 7 
	
	
HC1T7 - Task 7: Converting Fahrenheit to Celsius
Write a function fToC that converts Fahrenheit to Celsius.

-- Function to convert Fahrenheit to Celsius
fToC :: Float -> Float
fToC f = (5 / 9) * (f - 32)

-- Main function to test the conversion
main :: IO ()
main = do
    let fahrenheit = 98.6
    putStrLn $ "Fahrenheit: " ++ show fahrenheit
    putStrLn $ "Celsius: " ++ show (fToC fahrenheit)
	
HC1T8 - Task 8: Higher-Order Functions
Create a function applyTwice that applies a function twice to an input value.

-- Function that applies another function twice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)  

-- Main function to test applyTwice
main :: IO ()
main = do
   
    let double x = x * 2
    putStrLn "Applying 'double' twice to 5:"
    print (applyTwice double 5)
