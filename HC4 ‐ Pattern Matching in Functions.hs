HC4T1 - Task 1: Define a weatherReport Function
Create a function weatherReport :: String -> String that takes a weather condition as a string (e.g., "sunny", "rainy", "cloudy") and returns a message describing the weather. Use pattern matching to handle the following cases:

"sunny" → "It's a bright and beautiful day!"
"rainy" → "Don't forget your umbrella!"
"cloudy" → "A bit gloomy, but no rain yet!"
Any other input should return "Weather unknown".


-- Define the weatherReport function with pattern matching
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

main :: IO ()
main = do
  putStrLn (weatherReport "sunny")
  putStrLn (weatherReport "rainy")
  putStrLn (weatherReport "cloudy")
  putStrLn (weatherReport "windy") 
  
  
HC4T2 - Task 2: Define a dayType Function
Create a function dayType :: String -> String that determines if a given day of the week is a weekday or weekend.

"Saturday" and "Sunday" → "It's a weekend!"
Any other day of the week → "It's a weekday."
If an invalid day is provided, return "Invalid day".

dayType :: String -> String
dayType "saturday" = "It's a weekend!"
dayType "sunday"   = "It's a weekend!"
dayType "monday"   = "It's a weekday."
dayType "tuesday"  = "It's a weekday."
dayType "wednesday" = "It's a weekday."
dayType "thursday" = "It's a weekday."
dayType "friday"   = "It's a weekday."
dayType _          = "Invalid day"

main :: IO ()
main = do
  putStrLn (dayType "sunday")
  putStrLn (dayType "tuesday")
  putStrLn (dayType "holiday") 

  

HC4T3 - Task 3: Define a gradeComment Function
Write a function gradeComment :: Int -> String that takes a numerical grade and returns a comment based on the grade range:

90 - 100 → "Excellent!"
70 - 89 → "Good job!"
50 - 69 → "You passed."
0 - 49 → "Better luck next time."
Any other number → "Invalid grade".

-- Define the gradeComment function
gradeComment :: Int -> String
gradeComment grade
  | grade >= 90 && grade <= 100 = "Excellent!"
  | grade >= 70 && grade <= 89  = "Good job!"
  | grade >= 50 && grade <= 69  = "You passed."
  | grade >= 0 && grade <= 49   = "Better luck next time."
  | otherwise                   = "Invalid grade"

main :: IO ()
main = do
  putStrLn (gradeComment 95)   
  putStrLn (gradeComment 75)   
  putStrLn (gradeComment 60)   
  putStrLn (gradeComment 30)   
  putStrLn (gradeComment 150)  


HC4T4 - Task 4: Rewrite specialBirthday using Pattern Matching
Rewrite the specialBirthday function using pattern matching instead of if-else statements.

-- Define the specialBirthday function using pattern matching
specialBirthday :: Int -> String
specialBirthday 1  = "First birthday!"
specialBirthday 18 = "You're an adult!"
specialBirthday 60 = "Finally, I can stop caring about new lingo!"
specialBirthday _  = "Nothing special"

main :: IO ()
main = do
  putStrLn (specialBirthday 1)
  putStrLn (specialBirthday 18)
  putStrLn (specialBirthday 60)
  putStrLn (specialBirthday 25)  

  
  
HC4T5 - Task 5: Add a Catch-All Pattern with a Custom Message
Modify specialBirthday to include the age in the return message when it doesn’t match predefined cases.
  
-- Define the specialBirthday function with a catch-all pattern and custom message
specialBirthday :: Int -> String
specialBirthday 1 = "First birthday!"
specialBirthday 18 = "You're an adult!"
specialBirthday 60 = "Finally, I can stop caring about new lingo!"
specialBirthday age = "Nothing special about age " ++ show age  -- Custom message for other ages

main :: IO ()
main = do
  putStrLn (specialBirthday 1)
  putStrLn (specialBirthday 18)
  putStrLn (specialBirthday 60)
  putStrLn (specialBirthday 25)  
  
  

HC4T6 - Task 6: Identify List Contents Using Pattern Matching
Create a function whatsInsideThisList that returns a string based on the number of elements in a list.

whatsInsideThisList :: [a] -> String
whatsInsideThisList [] = "The list is empty."
whatsInsideThisList [x] = "The list has one element."
whatsInsideThisList [x, y] = "The list has two elements."
whatsInsideThisList _ = "The list has many elements."

main :: IO ()
main = do
  print (whatsInsideThisList [])             
  print (whatsInsideThisList [42])           
  print (whatsInsideThisList ["a", "b"])     
  print (whatsInsideThisList [1, 2, 3])  
  

HC4T7 - Task 7: Ignore Elements in a List
Modify firstAndThird to return only the first and third elements of a list, ignoring others.

firstAndThird :: [Bool] -> String
firstAndThird (x:_:z:_) = "The first and third elements are: " ++ show x ++ " and " ++ show z
firstAndThird _ = "Don't have them!"

main :: IO ()
main = do
  print (firstAndThird [True, True, False])      
  print (firstAndThird [False, True, True, True]) 
  print (firstAndThird [True])                   
    

HC4T8 - Task 8: Extract Values from Tuples
Create a function describeTuple that extracts values from a tuple and returns a string.

describeTuple :: (String, Int, Bool) -> String
describeTuple (name, age, isStudent) =
  name ++ " is " ++ show age ++ " years old and " ++ studentStatus
  where
    studentStatus = if isStudent then "is a student." else "is not a student."

main :: IO ()
main = do
  let info1 = ("Alice", 22, True)
  let info2 = ("Bob", 30, False)
  putStrLn (describeTuple info1)
  putStrLn (describeTuple info2)


 
