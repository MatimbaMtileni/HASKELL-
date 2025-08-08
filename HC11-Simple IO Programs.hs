-- HC11T1: Greet the User
-- Write a program that asks the user for their name and prints a greeting.

main :: IO ()
main = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")



-- HC11T2: Count Characters in a Line
-- Write a program that reads a line of input and prints the number of characters in that line.

main :: IO ()
main = do
  putStrLn "Enter a line:"
  line <- getLine
  putStrLn ("Character count: " ++ show (length line))



-- HC11T3: Double a Number
-- Write a program that asks the user for a number, reads it, and prints that number multiplied by 2.

main :: IO ()
main = do
  putStrLn "Enter a number:"
  input <- getLine
  let number = read input :: Int
  putStrLn ("Double: " ++ show (number * 2))




--HC11T4: Concatenate Two Lines
--Write a program that reads two lines of input and prints them concatenated.

main :: IO ()
main = do
  putStrLn "Enter the first line:"
  line1 <- getLine
  putStrLn "Enter the second line:"
  line2 <- getLine
  putStrLn ("Concatenated: " ++ line1 ++ line2)




-- HC11T5: Repeat Until "quit"
-- Write a program that repeatedly asks the user for input until they enter "quit".

main :: IO ()
main = do
  putStrLn "Type something (or 'quit' to exit):"
  loop

loop :: IO ()
loop = do
  input <- getLine
  if input == "quit"
    then putStrLn "Goodbye!"
    else do
      putStrLn ("You typed: " ++ input)
      loop




-- HC11T6: Uppercase Converter
-- Write a program that reads a line of input, converts it to uppercase, and prints it.

import Data.Char (toUpper)

main :: IO ()
main = do
  putStrLn "Enter text to convert to uppercase:"
  input <- getLine
  putStrLn (map toUpper input)




-- HC11T7: User Options
-- Write a program that prints a list of options to the user and executes different code based on the user's choice.

main :: IO ()
main = do
  putStrLn "Choose an option:"
  putStrLn "1. Greet"
  putStrLn "2. Add numbers"
  putStrLn "3. Exit"
  option <- getLine
  case option of
    "1" -> putStrLn "Hello there!"
    "2" -> do
      putStrLn "Enter first number:"
      a <- getLine
      putStrLn "Enter second number:"
      b <- getLine
      let sum = read a + read b :: Int
      putStrLn ("Sum: " ++ show sum)
    "3" -> putStrLn "Goodbye!"
    _   -> putStrLn "Invalid option."




-- HC11T8: Even or Odd Checker
-- Write a program that reads a number and tells the user if it's even or odd.

main :: IO ()
main = do
  putStrLn "Enter a number:"
  input <- getLine
  let n = read input :: Int
  if even n
    then putStrLn "That number is even."
    else putStrLn "That number is odd."




-- HC11T9: Sum Two Numbers
-- Write a program that reads two numbers and prints their sum.

main :: IO ()
main = do
  putStrLn "Enter first number:"
  a <- getLine
  putStrLn "Enter second number:"
  b <- getLine
  let sum = read a + read b :: Int
  putStrLn ("Sum: " ++ show sum)




-- HC11T10: Reverse User Input
-- Write a program that reads input from the user and reverses the string.

main :: IO ()
main = do
  putStrLn "Enter a string to reverse:"
  input <- getLine
  putStrLn ("Reversed: " ++ reverse input)

