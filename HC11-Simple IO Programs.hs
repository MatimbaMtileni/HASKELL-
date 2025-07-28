-- HC11T1: Greet the User
-- Write a program that asks the user for their name and prints a greeting.

main :: IO ()
main = greet "Matimba"  

greet :: String -> IO ()
greet name = putStrLn ("Hello, " ++ name ++ "!")


-- HC11T2: Count Characters in a Line
-- Write a program that reads a line of input and prints the number of characters in that line.

main :: IO ()
main = do
  let line = "Hello, Haskell"  
  let count = length line
  putStrLn ("Number of characters: " ++ show count)



-- HC11T3: Double a Number
-- Write a program that asks the user for a number, reads it, and prints that number multiplied by 2.

main :: IO ()
main = do
  let input = "7"  
  let number = read input :: Int
  putStrLn ("Double of your number is: " ++ show (number * 2))



-- HC11T4: Concatenate Two Lines
-- Write a program that reads two lines of input and prints them concatenated.

main :: IO ()
main = do
  let line1 = "Hello, "
  let line2 = "world!"
  putStrLn ("Concatenated line: " ++ line1 ++ line2)



-- HC11T5: Repeat Until "quit"
-- Write a program that repeatedly asks the user for input until they enter "quit".

main :: IO ()
main = do
  putStrLn "Type something (or type 'quit' to exit):"
  loopSim ["Matimba", "Mtileni", "quit"]

loopSim :: [String] -> IO ()
loopSim [] = putStrLn "Program ended."
loopSim (input:rest) =
  if input == "quit"
    then putStrLn "Program ended."
    else do
      putStrLn ("You said: " ++ input)
      loopSim rest



-- HC11T6: Uppercase Converter
-- Write a program that reads a line of input, converts it to uppercase, and prints it.

import Data.Char (toUpper)

main :: IO ()
main = do
  let input = "I'm learning haskell"
  let upper = map toUpper input
  putStrLn ("Uppercase version: " ++ upper)



-- HC11T7: User Options
-- Write a program that prints a list of options to the user and executes different code based on the user's choice.

main :: IO ()
main = menuLoop ["1", "2", "3", "hello", "4", "3"]

menuLoop :: [String] -> IO ()
menuLoop [] = putStrLn "No more inputs. Program ended."
menuLoop (choice:rest) = do
  putStrLn "Choose an option:"
  putStrLn "1. Greet"
  putStrLn "2. Add Two Numbers"
  putStrLn "3. Exit"
  putStrLn ("User selected: " ++ choice)
  case choice of
    "1" -> do
      putStrLn "Hello, user."
      menuLoop rest
    "2" -> case rest of
      (a:b:rest') -> do
        putStrLn ("Enter first number: " ++ a)
        putStrLn ("Enter second number: " ++ b)
        let sum = read a + read b :: Int
        putStrLn ("Sum is: " ++ show sum)
        menuLoop rest'
      _ -> putStrLn "Not enough inputs for addition."
    "3" -> putStrLn "Goodbye."
    _ -> do
      putStrLn "Invalid option selected."
      menuLoop rest



-- HC11T8: Even or Odd Checker
-- Write a program that reads a number and tells the user if it's even or odd.

main :: IO ()
main = do
  let input = "42"  
  let num = read input :: Int
  if even num
    then putStrLn "The number is even."
    else putStrLn "The number is odd."



-- HC11T9: Sum Two Numbers
-- Write a program that reads two numbers and prints their sum.

main :: IO ()
main = do
  let a = "10"  
  let b = "20"  
  let sum = read a + read b :: Int
  putStrLn ("Sum is: " ++ show sum)



-- HC11T10: Reverse User Input
-- Write a program that reads input from the user and reverses the string.

main :: IO ()
main = do
  let input = "coxygen global"
  putStrLn ("Reversed string: " ++ reverse input)
