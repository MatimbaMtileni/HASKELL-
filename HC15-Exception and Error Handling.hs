--HC15T1: Handle Exceptions for File Reading and Velocity Calculation
--Handle exceptions in a program that reads a file and calculates velocity based on user input.

import Control.Exception
import System.IO

main :: IO ()
main = do
  result <- try $ readFile "distance.txt" :: IO (Either IOException String)
  case result of
    Left e  -> putStrLn $ "ERROR: Could not read file - " ++ show e
    Right d -> do
      putStrLn "Enter time:"
      t <- getLine
      let velocity = (read d :: Int) `div` read t
      putStrLn $ "Velocity: " ++ show velocity


--HC15T2: Self-Driving AI Car System
--Implement a basic self-driving AI car system that reacts to traffic light colors.

main :: IO ()
main = dumbAICar

dumbAICar :: IO ()
dumbAICar = do
  putStrLn "What color is the traffic light?"
  color <- getLine
  putStrLn $ "Then I'll " ++ decide color
  dumbAICar

decide :: String -> String
decide "Red"    = "Stop!"
decide "Yellow" = "Wait!"
decide "Green"  = "Go!"
decide _        = "Do nothing. Invalid signal."


--HC15T3: Custom Exception for Traffic Light Errors
--Define and throw a custom exception for traffic light errors.

import Control.Exception

data TrafficLightException = LightOff | UnknownColor String deriving (Show)
instance Exception TrafficLightException

throwTrafficError :: String -> IO ()
throwTrafficError "Black" = throwIO LightOff
throwTrafficError color   = throwIO (UnknownColor color)

main :: IO ()
main = do
  putStrLn "Enter traffic light color:"
  color <- getLine
  throwTrafficError color


--HC15T4: Exception Handler for Traffic Light
--Use a handler function to catch and handle traffic light exceptions.

import Control.Exception

main :: IO ()
main = dumbAICar

dumbAICar :: IO ()
dumbAICar = do
  putStrLn "Traffic light color:"
  color <- getLine
  action <- (nextMove color) `catch` handler
  putStrLn $ "Action: " ++ action
  dumbAICar

data TrafficLightException = LightOff | UnknownColor String deriving (Show)
instance Exception TrafficLightException

nextMove :: String -> IO String
nextMove "Red"    = return "Stop!"
nextMove "Green"  = return "Go!"
nextMove "Yellow" = return "Wait!"
nextMove "Black"  = throwIO LightOff
nextMove color    = throwIO $ UnknownColor color

handler :: TrafficLightException -> IO String
handler LightOff           = return "Proceed with caution!"
handler (UnknownColor col) = return $ "Stop! Unknown color: " ++ col


--HC15T5: Safe Division Using Maybe
--Write a safe division function using the Maybe type to avoid divide-by-zero errors.

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

showResult :: Maybe Int -> String
showResult Nothing  = "Error: Division by zero!"
showResult (Just v) = "Result: " ++ show v

main :: IO ()
main = do
  putStrLn "Enter numerator:"
  nStr <- getLine
  putStrLn "Enter denominator:"
  dStr <- getLine
  let n = read nStr :: Int
      d = read dStr :: Int
  putStrLn $ showResult (safeDiv n d)

--HC15T6: Safe Input Parsing with readMaybe
--Use readMaybe to parse user input safely and avoid runtime errors during input parsing.

import Text.Read (readMaybe)

getInt :: String -> IO (Maybe Int)
getInt prompt = do
  putStrLn prompt
  input <- getLine
  return $ readMaybe input

main :: IO ()
main = do
  num1 <- getInt "Enter first number:"
  num2 <- getInt "Enter second number:"
  putStrLn $ "First parsed: "  ++ show num1
  putStrLn $ "Second parsed: " ++ show num2

--HC15T7: Velocity Calculation with Optionals and Parsing Handling
--Implement a program that calculates velocity using optional values and handles parsing errors.

import Text.Read (readMaybe)

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

vel :: Maybe Int -> Maybe Int -> Maybe Int
vel (Just d) (Just t) = safeDiv d t
vel _ _ = Nothing

main :: IO ()
main = do
  putStrLn "Enter distance:"
  dStr <- getLine
  putStrLn "Enter time:"
  tStr <- getLine
  let result = vel (readMaybe dStr) (readMaybe tStr)
  case result of
    Just v  -> putStrLn $ "Velocity: " ++ show v
    Nothing -> putStrLn "Invalid input or divide by zero!"


--HC15T8: Division with Either for Detailed Errors
--Define a function using the Either type to provide detailed error messages for division.

safeDivE :: Int -> Int -> Either String Int
safeDivE _ 0 = Left "Cannot divide by zero"
safeDivE x y = Right (x `div` y)

showResult :: Either String Int -> String
showResult (Left err)  = "Error: " ++ err
showResult (Right val) = "Result: " ++ show val

main :: IO ()
main = do
  putStrLn "Enter numerator:"
  nStr <- getLine
  putStrLn "Enter denominator:"
  dStr <- getLine
  let n = readEither nStr :: Either String Int
      d = readEither dStr :: Either String Int
  case (n, d) of
    (Right x, Right y) -> putStrLn $ showResult (safeDivE x y)
    (Left err, _)      -> putStrLn $ "Invalid numerator: " ++ err
    (_, Left err)      -> putStrLn $ "Invalid denominator: " ++ err

--HC15T9: Try Function for File IO Exceptions
--Use the try function to catch file IO exceptions and handle them gracefully.

import Control.Exception
import System.IO

main :: IO ()
main = do
  result <- try $ readFile "data.txt" :: IO (Either IOException String)
  case result of
    Left e  -> putStrLn $ "File error: " ++ show e
    Right content -> putStrLn $ "File content:\n" ++ content


--HC15T10: Hybrid Error Handling with Either and IO
--Create a velocity program that uses both Either and IO exceptions to demonstrate hybrid error handling.

import Control.Exception
import Text.Read (readEither)
import System.IO

safeDivE :: Int -> Int -> Either String Int
safeDivE _ 0 = Left "Time can't be zero!"
safeDivE x y = Right (x `div` y)

vel :: Either String Int -> Either String Int -> Either String Int
vel (Right d) (Right t) = safeDivE d t
vel (Left d) (Left t) = Left $ "Both inputs invalid: d=" ++ d ++ ", t=" ++ t
vel (Left d) _ = Left $ "Distance error: " ++ d
vel _ (Left t) = Left $ "Time error: " ++ t

main :: IO ()
main = do
  fileResult <- try $ readFile "distance.txt" :: IO (Either IOException String)
  case fileResult of
    Left e -> putStrLn $ "File error: " ++ show e
    Right dStr -> do
      putStrLn "Enter time:"
      tStr <- getLine
      let result = vel (readEither dStr) (readEither tStr)
      case result of
        Right v -> putStrLn $ "Velocity: " ++ show v
        Left err -> putStrLn $ "Error: " ++ err
