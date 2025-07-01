-- HC6T1: Implement a recursive function to compute the factorial of a number.
factorial :: Int -> Int
factorial 0 = 1                           
factorial n = n * factorial (n - 1)       

main :: IO ()
main = do
    putStrLn ("factorial 0 = " ++ show (factorial 0))
    putStrLn ("factorial 1 = " ++ show (factorial 1))
    putStrLn ("factorial 2 = " ++ show (factorial 2))
    putStrLn ("factorial 3 = " ++ show (factorial 3))
    putStrLn ("factorial 4 = " ++ show (factorial 4))
	
-- HC6T2: Implement a recursive function to compute the nth Fibonacci number.

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main :: IO ()
main = print (fibonacci 7)

-- HC6T3: Implement a function to compute the sum of elements in a list using foldr.

sumFoldr :: [Int] -> Int
sumFoldr = foldr (+) 0

main :: IO ()
main = print (sumFoldr [1, 2, 3, 4])  


-- HC6T4: Implement a function to compute the product of elements in a list using foldl.

productFoldl :: [Int] -> Int
productFoldl = foldl (*) 1

main :: IO ()
main = print (productFoldl [1, 2, 3, 4])  

-- HC6T5: Implement a function that reverses a list using recursion.

reverseRec :: [a] -> [a]
reverseRec []     = []
reverseRec (x:xs) = reverseRec xs ++ [x]

main :: IO ()
main = print (reverseRec [1, 2, 3])  

-- HC6T6: Implement a function that determines whether a given element exists in a list.

elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists e (x:xs)
    | e == x    = True
    | otherwise = elementExists e xs

main :: IO ()
main = print (elementExists 3 [1, 2, 4, 3])  

-- HC6T7: Implement a function that takes a list and returns the length of the list.

listLength :: [a] -> Int
listLength []     = 0
listLength (_:xs) = 1 + listLength xs

main :: IO ()
main = print (listLength [10, 20, 30])  

-- HC6T8: Implement a function that filters all even numbers from a list.

filterEvens :: [Int] -> [Int]
filterEvens = filter even

main :: IO ()
main = print (filterEvens [1..10])  

-- HC6T9: Implement a function that applies a given function to each element of a list (map implementation).

mapFunc :: (a -> b) -> [a] -> [b]
mapFunc _ []     = []
mapFunc f (x:xs) = f x : mapFunc f xs

main :: IO ()
main = print (mapFunc (*2) [1, 2, 3])  

-- HC6T10: Implement a recursive function that takes a number and returns a list of its digits.

digits :: Integer -> [Integer]
digits n
    | n < 10    = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]

main :: IO ()
main = print (digits 12345)  
