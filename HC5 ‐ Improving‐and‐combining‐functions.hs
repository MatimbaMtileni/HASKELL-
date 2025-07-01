HC5T1: Using applyTwice
Define a function that takes a function and an integer, then applies the function three times to the integer.

-- Define a function that applies another function three times
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

double :: Int -> Int
double x = x * 2

main :: IO ()
main = do
    let result = applyThrice double 1
    putStrLn ("Result of applying three times to the integer: " ++ show result)


HC5T2: Filtering Odd Numbers
Use the filter function to extract all odd numbers from a given list of integers from 1 to 30.

-- Function to filter odd numbers from a list
filterOddNumbers :: [Int] -> [Int]
filterOddNumbers xs = filter odd xs

main :: IO ()
main = do
    let numbers = [1..30]
    let oddNumbers = filterOddNumbers numbers
    putStrLn "Odd numbers from 1 to 30:"
    print oddNumbers
	
	
HC5T3: Checking for Uppercase Letters
Write a function using any that checks if a list of words contains any word that starts with an uppercase letter.


isUppercaseLetter :: Char -> Bool
isUppercaseLetter c = c >= 'A' && c <= 'Z'

containsUppercaseStart :: [String] -> Bool
containsUppercaseStart wordsList = any startsWithUpper wordsList
  where
    startsWithUpper []    = False
    startsWithUpper (x:_) = isUppercaseLetter x

main :: IO ()
main = do
    let sample1 = ["apple", "banana", "Cherry", "date"]
    let sample2 = ["apple", "banana", "cherry", "date"]

    putStrLn ("Sample 1 has word starting with uppercase: " ++ show (containsUppercaseStart sample1))
    putStrLn ("Sample 2 has word starting with uppercase: " ++ show (containsUppercaseStart sample2))


HC5T4: Using Lambda Functions
Rewrite the following function using a lambda function:

biggerThan10 x = x > 10

-- Define the function using a lambda expression
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

main :: IO ()
main = do
    putStrLn ("Is 5 bigger than 10? " ++ show (biggerThan10 5))
    putStrLn ("Is 15 bigger than 10? " ++ show (biggerThan10 15))
	
	
HC5T5: Partial Application
Create a function multiplyByFive that uses partial application to multiply any number by 5.

-- Define a partially applied function to multiply by 5
multiplyByFive :: Int -> Int
multiplyByFive = (*) 5

main :: IO ()
main = do
    putStrLn ("5 * 3 = " ++ show (multiplyByFive 3))
    putStrLn ("5 * 10 = " ++ show (multiplyByFive 10))
	
	
HC5T6: Function Composition
Use function composition (.) to create a function that takes a list of numbers and returns their squares filtered to only keep the even ones.

-- Define the composed function: squares then filters even numbers
evenSquares :: [Int] -> [Int]
evenSquares = filter even . map square
  where
    square x = x * x

main :: IO ()
main = do
    let numbers = [1..10]
    putStrLn "Even squares from 1 to 10:"
    print (evenSquares numbers)
	
	
HC5T7: The $ Operator
Rewrite the following function using the $ operator:

result = sum (map (*2) (filter (>3) [1..10]))

-- Rewriting using the $ operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

main :: IO ()
main = do
    putStrLn ("The result is: " ++ show result)
	
	
HC5T8: Point-Free Style
Convert the following function to point-free style:

addFive x = x + 5

addFive :: Int -> Int
addFive = (+ 5)

main :: IO ()
main = do
    putStrLn ("5 + 5 = " ++ show (addFive 5))
    putStrLn ("10 + 5 = " ++ show (addFive 10))
	

HC5T9: Higher-Order Function to Transform a List
Write a higher-order function transformList that applies a given function twice to every element of a list.

-- Higher-order function: applies a given function twice to each list element
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

addOne :: Int -> Int
addOne x = x + 1

main :: IO ()
main = do
    let originalList = [1, 2, 3, 4]
    let transformed = transformList addOne originalList
    putStrLn "Original list:"
    print originalList
    putStrLn "Transformed list:"
    print transformed
	
	
	
HC5T10: Combining Higher-Order Functions
Combine filter, map, and any to create a function that checks if any squared value in a list is greater than 50.

-- Function to check if any squared value is greater than 50
hasLargeSquare :: [Int] -> Bool
hasLargeSquare = any (>50) . map (^2)

main :: IO ()
main = do
    let list1 = [2, 4, 5, 6]     
    let list2 = [5, 8, 9, 10]    

    putStrLn ("List1 has square > 50: " ++ show (hasLargeSquare list1))
    putStrLn ("List2 has square > 50: " ++ show (hasLargeSquare list2))







