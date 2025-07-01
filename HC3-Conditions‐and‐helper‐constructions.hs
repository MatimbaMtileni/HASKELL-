HC3T1 - Task 1: Check if a number is positive, negative, or zero
1. Define a function checkNumber :: Int -> String.
2. Use an if-then-else statement to check if the number is positive, negative, or zero.
3. Return "Positive", "Negative", or "Zero" accordingly.
4. Test your function with checkNumber 5, checkNumber (-3), and checkNumber 0.

-- Function to check if a number is positive, negative, or zero
checkNumber :: Int -> String
checkNumber x =
  if x > 0 then "Positive"
  else if x < 0 then "Negative"
  else "Zero"

main :: IO ()
main = do
  print (checkNumber 5)     -- should print "Positive"
  print (checkNumber (-3))  -- should print "Negative"
  print (checkNumber 0)     -- should print "Zero"
  
  
HC3T2 - Task 2: Determine the grade based on a score using guards
1. Define a function grade :: Int -> String.
2. Use guards (|) to classify scores into grades:
90 and above: "A"
80 to 89: "B"
70 to 79: "C"
60 to 69: "D"
Below 60: "F"
3. Test your function with grade 95, grade 72, and grade 50.

-- Function to determine grade based on score using guards
grade :: Int -> String
grade score
  | score >= 90 = "A"
  | score >= 80 = "B"
  | score >= 70 = "C"
  | score >= 60 = "D"
  | otherwise   = "F"

main :: IO ()
main = do
  print (grade 95)  
  print (grade 72)  
  print (grade 50)  


HC3T3 - Task 3: Convert an RGB color to a hex string using let bindings
1. Define a function rgbToHex :: (Int, Int, Int) -> String.
2. Use let bindings to format each color component as a two-character hex string.
3. Concatenate the three hex values into a single string.
4. Test your function with rgbToHex (255, 0, 127) and rgbToHex (0, 255, 64).

-- Function to convert a single Int (0–255) to a 2-character hex string
toHex :: Int -> String
toHex n = let hex = "0123456789ABCDEF"
              high = n `div` 16
              low = n `mod` 16
          in [hex !! high, hex !! low]

-- Main function to convert RGB to hex string 
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
  let redHex   = toHex r
      greenHex = toHex g
      blueHex  = toHex b
  in "#" ++ redHex ++ greenHex ++ blueHex

main :: IO ()
main = do
  print (rgbToHex (255, 0, 127))   
  print (rgbToHex (0, 255, 64)) 


HC3T4 - Task 4: Calculate the area of a triangle using Heron's formula
1.Define a function triangleArea :: Float -> Float -> Float -> Float. 
2.Use let to calculate the semi-perimeter s. 
3.Apply Heron’s formula: sqrt(s * (s - a) * (s - b) * (s - c)). 
4.Test with triangleArea 3 4 5 and triangleArea 7 8 9.

-- Function to calculate the area of a triangle using Heron's formula
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
  let s = (a + b + c) / 2
  in sqrt (s * (s - a) * (s - b) * (s - c))

main :: IO ()
main = do
  print (triangleArea 3 4 5)   
  print (triangleArea 7 8 9)  


HC3T5 - Task 5: Determine the type of a triangle using guards
1.Define a function triangleType :: Float -> Float -> Float -> String. 
2.Use guards to classify the triangle: 
All sides equal: "Equilateral"
Two sides equal: "Isosceles"
No sides equal: "Scalene"
3.Test with triangleType 3 3 3, triangleType 5 5 8, and triangleType 6 7 8. 

-- Function to determine the type of a triangle
triangleType :: Float -> Float -> Float -> String
triangleType a b c
  | a == b && b == c          = "Equilateral"
  | a == b || b == c || a == c = "Isosceles"
  | otherwise                  = "Scalene"

main :: IO ()
main = do
  print (triangleType 3 3 3)   
  print (triangleType 5 5 8)   
  print (triangleType 6 7 8)   



HC3T6 - Advanced Task 6: Check leap year using if-then-else
1. Define isLeapYear :: Int -> Bool. 
2. Use if-then-else to check: 
Divisible by 400: True
Divisible by 100 but not 400: False
Divisible by 4: True
Otherwise: False
3. Test with isLeapYear 2000, isLeapYear 1900, and isLeapYear 2024.

-- Function to check if a year is a leap year
isLeapYear :: Int -> Bool
isLeapYear year =
  if year `mod` 400 == 0 then True
  else if year `mod` 100 == 0 then False
  else if year `mod` 4 == 0 then True
  else False

main :: IO ()
main = do
  print (isLeapYear 2000)  -- True (divisible by 400)
  print (isLeapYear 1900)  -- False (divisible by 100 but not 400)
  print (isLeapYear 2024)  -- True (divisible by 4)


HC3T7 - Advanced Task 7: Determine the season based on the month using guards
1. Define season :: Int -> String. 
2. Use guards to return: 
12, 1, 2 -> "Winter"
3, 4, 5 -> "Spring"
6, 7, 8 -> "Summer"
9, 10, 11 -> "Autumn"
3. Test with season 3, season 7, and season 11.

-- Function to determine the season based on the month
season :: Int -> String
season m
  | m == 12 || m == 1 || m == 2 = "Winter"
  | m == 3 || m == 4 || m == 5 = "Spring"
  | m == 6 || m == 7 || m == 8 = "Summer"
  | m == 9 || m == 10 || m == 11 = "Autumn"
  | otherwise = "Invalid month"

main :: IO ()
main = do
  print (season 3)   
  print (season 7)   
  print (season 11)  


HC3T8 - Advanced Task 8: Calculate BMI and return category using where
1. Define bmiCategory :: Float -> Float -> String. 
2. Use where to calculate BMI: bmi = weight / height^2. 
3. Use guards to classify BMI: 
<18.5: "Underweight"
18.5 to 24.9: "Normal"
25 to 29.9: "Overweight"
=30: "Obese"
4. Test with bmiCategory 70 1.75 and bmiCategory 90 1.8.

-- Define the bmiCategory function
bmiCategory :: Float -> Float -> String
bmiCategory weight height
  | bmi < 18.5 = "Underweight"
  | bmi < 25.0 = "Normal"
  | bmi < 30.0 = "Overweight"
  | otherwise  = "Obese"
  where
    bmi = weight / height^2

main :: IO ()
main = do
  putStrLn ("BMI category for 70kg and 1.75m: " ++ bmiCategory 70 1.75)
  putStrLn ("BMI category for 90kg and 1.8m: "  ++ bmiCategory 90 1.8)


HC3T9 - Advanced Task 9: Find the maximum of three numbers using let
1. Define maxOfThree :: Int -> Int -> Int -> Int. 
2. Use let to store intermediate max values. 
3. Return the maximum of the three numbers. 
4. Test with maxOfThree 10 20 15 and maxOfThree 5 25 10.


-- Define the maxOfThree function
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z =
  let
    max1 = if x > y then x else y
    max2 = if max1 > z then max1 else z
  in
    max2

main :: IO ()
main = do
  putStrLn ("Maximum of 10, 20, 15: " ++ show (maxOfThree 10 20 15))
  putStrLn ("Maximum of 5, 25, 10: "  ++ show (maxOfThree 5 25 10))


HC3T10 - Advanced Task 10: Check if a string is a palindrome using recursion and guards
1. Define isPalindrome :: String -> Bool. 
2. Use guards: 
If the string has 0 or 1 characters: True
If the first and last characters match, recursively check the rest.
Otherwise, return False.
3. Test with isPalindrome "racecar", isPalindrome "haskell", and isPalindrome "madam".

-- Define the isPalindrome function
isPalindrome :: String -> Bool
isPalindrome str
  | length str <= 1 = True
  | head str == last str = isPalindrome (init (tail str))
  | otherwise = False

main :: IO ()
main = do
  putStrLn ("Is 'racecar' a palindrome? " ++ show (isPalindrome "racecar"))
  putStrLn ("Is 'haskell' a palindrome? " ++ show (isPalindrome "haskell"))
  putStrLn ("Is 'madam' a palindrome? " ++ show (isPalindrome "madam"))



