HC7 ‐ Haskell Chapter 7 Practical Tasks: Intro‐to‐Type‐Classes

HC7T1: Implement an Eq instance for a custom data type
Define a data type Color representing Red, Green, and Blue. Implement the Eq type class for it so that colors of the same type are considered equal.

module Main where

data Color = Red | Green | Blue

instance Eq Color where
    Red == Red     = True
    Green == Green = True
    Blue == Blue   = True
    _ == _         = False

main :: IO ()
main = do
    print (Red == Red)    
    print (Red == Blue)    
    print (Green == Green) 
    print (Blue == Green)  

HC7T2: Implement an Ord instance for a custom data type
Using the Color type from HC7T1, implement the Ord type class so that Red < Green < Blue.

module Main where

data Color = Red | Green | Blue deriving (Eq, Show)

instance Ord Color where
    compare Red Red     = EQ
    compare Red _       = LT
    compare Green Red   = GT
    compare Green Green = EQ
    compare Green Blue  = LT
    compare Blue Blue   = EQ
    compare Blue _      = GT

main :: IO ()
main = do
    print (Red < Green)     
    print (Green < Blue)       
    print (Blue > Red)         
    print (compare Red Blue)   
    print (compare Green Blue) 
    print (compare Blue Blue)  

HC7T3: Implement a function using multiple constraints
Write a function compareValues that takes two arguments of type a and returns the larger one. Ensure that a is both an instance of Eq and Ord.


compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y
  | x >= y    = x
  | otherwise = y


main :: IO ()
main = do
  putStrLn "Comparing Integers:"
  print (compareValues 10 20)    

  putStrLn "Comparing Floats:"
  print (compareValues 5.5 3.2) 

  putStrLn "Comparing Strings:"
  print (compareValues "apple" "banana") 


HC7T4: Create a custom type and implement Show and Read
Define a data type Shape with constructors Circle Double and Rectangle Double Double. Implement Show and Read instances for it.


data Shape = Circle Double | Rectangle Double Double
    deriving (Eq)

instance Show Shape where
    show (Circle r) = "Circle " ++ show r
    show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h
    
instance Read Shape where
    readsPrec _ input =
        case words input of
            ("Circle":r:[]) -> [(Circle (read r), "")]
            ("Rectangle":w:h:[]) -> [(Rectangle (read w) (read h), "")]
            _ -> []


main :: IO ()
main = do
    let shape1 = Circle 3.5
    let shape2 = Rectangle 4.0 5.0

    putStrLn "Testing Show:"
    print shape1                        
    print shape2                       

    putStrLn "\nTesting Read:"
    let shapeStr1 = "Circle 6.7"
    let shapeStr2 = "Rectangle 2.2 3.3"

    let parsedShape1 = read shapeStr1 :: Shape
    let parsedShape2 = read shapeStr2 :: Shape

    print parsedShape1                 
    print parsedShape2                


HC7T5: Implement a function that uses Num constraints
Write a function squareArea that calculates the area of a square given its side length. Ensure that the function works with any numeric type.

squareArea :: Num a => a -> a
squareArea side = side * side

main :: IO ()
main = do
    let intSide = 5 :: Integer
    putStrLn $ "Square area (Integer): " ++ show (squareArea intSide)

    let floatSide = 4.2 :: Float
    putStrLn $ "Square area (Float): " ++ show (squareArea floatSide)

    let doubleSide = 6.5 :: Double
    putStrLn $ "Square area (Double): " ++ show (squareArea doubleSide)
	

HC7T6: Use the Integral and Floating type classes
Define a function circleCircumference that takes a radius and returns the circumference. Ensure it works with both Integral and Floating numbers.


circleCircumference :: (Real a, Floating b) => a -> b
circleCircumference radius = 2 * pi * (realToFrac radius)

main :: IO ()
main = do
  
    let radiusInt = 5 :: Integer
    putStrLn $ "Circumference (Integer): " ++ show (circleCircumference radiusInt)

    let radiusFloat = 4.5 :: Float
    putStrLn $ "Circumference (Float): " ++ show (circleCircumference radiusFloat)

    let radiusDouble = 6.2 :: Double
    putStrLn $ "Circumference (Double): " ++ show (circleCircumference radiusDouble)


HC7T7: Implement a function using Bounded and Enum
Create a function nextColor that takes a Color and returns the next color in sequence. If it reaches the last color, it should wrap around.

data Color = Red | Green | Blue | Yellow
    deriving (Show, Read, Eq, Enum, Bounded)

nextColor :: Color -> Color
nextColor color
    | color == maxBound = minBound  
    | otherwise = succ color        

main :: IO ()
main = do
    print $ nextColor Red     
    print $ nextColor Green   
    print $ nextColor Blue    
    print $ nextColor Yellow

HC7T8: Use Read to parse a value from a string
Write a function parseShape that takes a String and returns a Shape. The function should return Nothing for invalid inputs.

data Shape = Circle | Square | Triangle deriving (Show, Read, Eq)

parseShape :: String -> Maybe Shape
parseShape str =
  case reads str :: [(Shape, String)] of
    [(s, "")] -> Just s
    _         -> Nothing

main :: IO ()
main = do
  print $ parseShape "Circle"    
  print $ parseShape "Square"    
  print $ parseShape "Triangle"  
  print $ parseShape "circle" 
  print $ parseShape "Hexagon"   
  print $ parseShape "Circle!"   

HC7T9: Define a type class with multiple instances
Create a type class Describable with a method describe. Implement it for Bool and Shape.

data Shape = Circle | Square | Triangle deriving (Show, Read, Eq)

class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True  = "This is true."
  describe False = "This is false."

instance Describable Shape where
  describe Circle   = "A shape with no corners."
  describe Square   = "A shape with four equal sides."
  describe Triangle = "A shape with three sides."

main :: IO ()
main = do
  print $ describe True          
  print $ describe False          
  print $ describe Circle         
  print $ describe Square        
  print $ describe Triangle
  

HC7T10: Use a function with multiple type class constraints
Write a function describeAndCompare that takes two Describable values and returns the description of the larger one.

data Shape = Circle | Square | Triangle deriving (Show, Read, Eq, Ord)

class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True  = "This is true."
  describe False = "This is false."

instance Describable Shape where
  describe Circle   = "A shape with no corners."
  describe Square   = "A shape with four equal sides."
  describe Triangle = "A shape with three sides."

describeAndCompare :: (Ord a, Describable a) => a -> a -> String
describeAndCompare x y = describe (max x y)

main :: IO ()
main = do
  
  print $ describeAndCompare False True        
  print $ describeAndCompare Circle Square     
  print $ describeAndCompare Triangle Circle  
