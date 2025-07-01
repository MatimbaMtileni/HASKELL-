HC2T1 - Task 1: Checking Types in GHCi
Open GHCi and check the types of the following expressions:

42
3.14
"Haskell"
'Z'
True && False


-- Integer (Num a => a)
x :: Integer
x = 42

-- Fractional (Fractional a => a)
y :: Double
y = 3.14

-- String ([Char])
name :: String
name = "Haskell"

-- Char
initial :: Char
initial = 'Z'

-- Bool
logic :: Bool
logic = True && False

main :: IO ()
main = do
    putStrLn ("x (Integer) = " ++ show x)
    putStrLn ("y (Double) = " ++ show y)
    putStrLn ("name (String) = " ++ name)
    putStrLn ("initial (Char) = " ++ [initial])
    putStrLn ("logic (Bool) = " ++ show logic)


HC2T2 - Task 2: Function Type Signatures
1. Write function signatures for the following functions:

A function add that takes two Int values and returns their sum.
A function isEven that takes an Int and returns a Bool indicating if it's even.
A function concatStrings that takes two String values and returns their concatenation.

2.Implement these functions.

-- A function add that takes two Int values and returns their sum
add :: Int -> Int -> Int
add x y = x + y

-- A function isEven that takes an Int and returns a Bool indicating if it's even
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

-- A function concatStrings that takes two String values and returns their concatenation
concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

-- Main function to test the above
main :: IO ()
main = do
    putStrLn ("add 7 5 = " ++ show (add 7 5))
    putStrLn ("isEven 8 = " ++ show (isEven 8))
    putStrLn ("isEven 9 = " ++ show (isEven 9))
    putStrLn ("concatStrings \"Haskell, \" \" lesson\" = " ++ concatStrings "Haskell, " " lesson")
	

HC2T3 - Task 3: Immutable Variables
1. Define the following immutable variables in Haskell:

myAge as an Int
piValue as a Double
greeting as a String
isHaskellFun as a Bool

2. Try modifying one of the variables and observe what happens.

-- Defining immutable variables
myAge :: Int
myAge = 25

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

-- Main function to print the values
main :: IO ()
main = do
    putStrLn ("My age is: " ++ show myAge)
    putStrLn ("Value of pi is: " ++ show piValue)
    putStrLn greeting
    putStrLn ("Is Haskell fun? " ++ show isHaskellFun)
	
	
--The output of the modified variable

--Main.hs:17:1: error:
--    Multiple declarations of ‘myAge’
--    Declared at: Main.hs:5:1
--                 Main.hs:17:1


HC2T4 - Task 4: Converting Between Infix and Prefix Notations
1. Use prefix notation for the following infix expressions:

5 + 3
10 * 4
True && False

2. Use infix notation for the following prefix functions:

(+) 7 2
(*) 6 5
(&&) True False	


-- Prefix equivalents of infix expressions
prefixSum :: Int
prefixSum = (+) 5 3

prefixProduct :: Int
prefixProduct = (*) 10 4

prefixAnd :: Bool
prefixAnd = (&&) True False

-- Infix equivalents of prefix expressions
infixSum :: Int
infixSum = 7 + 2

infixProduct :: Int
infixProduct = 6 * 5

infixAnd :: Bool
infixAnd = True && False

-- Main function to display results
main :: IO ()
main = do
    putStrLn $ "prefixSum:" ++ show prefixSum
    putStrLn $ "prefixProduct:" ++ show prefixProduct
    putStrLn $ "prefixAnd:" ++ show prefixAnd

    putStrLn $ "infixSum:" ++ show infixSum
    putStrLn $ "infixProduct:" ++ show infixProduct
    putStrLn $ "infixAnd:" ++ show infixAnd
			 

HC2T5 - Task 5: Defining and Using Functions
1. Write a function circleArea that takes a Float radius and returns the area of the circle.
2. Write a function maxOfThree that takes three Int values and returns the maximum.
3. Test your functions with different inputs.

-- Calculate circle area
circleArea :: Float -> Float
circleArea radius = pi * radius * radius

-- Find max of three integers
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c = max a (max b c)

-- Test functions
main :: IO ()
main = do
    putStrLn $ "Area of circle with radius 3.0: " ++ show (circleArea 3.0)
    putStrLn $ "Area of circle with radius 5.5: " ++ show (circleArea 5.5)

    putStrLn $ "Max of (3, 7, 5): " ++ show (maxOfThree 3 7 5)
    putStrLn $ "Max of (10, 2, 8): " ++ show (maxOfThree 10 2 8)
    putStrLn $ "Max of (1, 1, 1): " ++ show (maxOfThree 1 1 1)


HC2T6 - Task 6: Understanding Int vs Integer
1. Define an Int variable smallNumber with the value 2^62.
2. Define an Integer variable bigNumber with the value 2^127.
3. Try to evaluate 2^64 :: Int in GHCi and note the result.

-- A large number within Int range
smallNumber :: Int
smallNumber = 2 ^ 62

-- A much larger number using Integer
bigNumber :: Integer
bigNumber = 2 ^ 127

main :: IO ()
main = do
    putStrLn $ "Int (2^62): " ++ show smallNumber
    putStrLn $ "Integer (2^127): " ++ show bigNumber

    let overflowTest = 2^64 :: Int
    putStrLn $ "2^64 as Int: " ++ show overflowTest
	
	
HC2T7 - Task 7: Boolean Expressions
1. Write Boolean expressions that evaluate to:
True using &&
False using ||
True using not
A comparison that returns False

main :: IO ()
main = do
    -- True using &&
    let expr1 = True && True
    putStrLn $ "True using && : " ++ show expr1

    -- False using ||
    let expr2 = False || False
    putStrLn $ "False using || : " ++ show expr2

    -- 3. True using not
    let expr3 = not False
    putStrLn $ "True using not : " ++ show expr3

    -- Comparison that returns False
    let expr4 = 5 > 10
    putStrLn $ "Comparison (5 > 10) returns : " ++ show expr4