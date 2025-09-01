--HC20T1: safeDivide with Maybe Monad
--Create a function safeDivide that performs safe division using the Maybe monad.

safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

main :: IO ()
main = do
    print $ safeDivide 10 2   
    print $ safeDivide 10 0   


--HC20T2: sequenceMaybe for List of Maybe
--Implement a function sequenceMaybe that converts a list of Maybe values into a Maybe of list.

sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe [] = Just []
sequenceMaybe (x:xs) = do
    val  <- x
    rest <- sequenceMaybe xs
    return (val : rest)

main :: IO ()
main = do
    print $ sequenceMaybe [Just 1, Just 2, Just 3]  
    print $ sequenceMaybe [Just 1, Nothing, Just 3] 


--HC20T3: Writer Monad Logging Calculator
--Create a logging calculator using the Writer monad to keep track of all operations.

import Control.Monad.Writer

add :: Int -> Int -> Writer [String] Int
add x y = writer (x + y, ["Added " ++ show x ++ " and " ++ show y])

multiply :: Int -> Int -> Writer [String] Int
multiply x y = writer (x * y, ["Multiplied " ++ show x ++ " and " ++ show y])

calculator :: Writer [String] Int
calculator = do
    a <- add 2 3
    b <- multiply a 4
    return b

main :: IO ()
main = do
    let (result, log) = runWriter calculator
    print result   
    mapM_ putStrLn log


--HC20T4: countChars with State Monad
--Implement a function countChars that counts occurrences of a character in a string using the State monad.

import Control.Monad.State
import qualified Data.Map as Map

type CharCount = Map.Map Char Int

countChars :: String -> State CharCount ()
countChars [] = return ()
countChars (c:cs) = do
    counts <- get
    let newCounts = Map.insertWith (+) c 1 counts
    put newCounts
    countChars cs

main :: IO ()
main = do
    let result = execState (countChars "banana") Map.empty
    print result  


--HC20T5: Reader Monad for Configurable Greeting
--Use the Reader monad to create a configuration-based system for greeting users.

import Control.Monad.Reader

type Config = String  

greet :: String -> Reader Config String
greet name = do
    prefix <- ask
    return $ prefix ++ ", " ++ name ++ "!"

main :: IO ()
main = do
    let greeting = runReader (greet "Matimba") "Hello"
    putStrLn greeting   


--HC20T6: doubleMonad Combining Maybe and List
--Implement a function doubleMonad that combines two monads: Maybe and List.

doubleMonad :: Maybe a -> [a] -> [Maybe a]
doubleMonad Nothing _  = [Nothing]
doubleMonad (Just x) xs = map Just xs

main :: IO ()
main = do
    print $ doubleMonad (Just 5) [1,2,3]   
    print $ doubleMonad Nothing [1,2,3]    


--HC20T7: findFirst with Either Monad
--Create a function findFirst that uses the Either monad to handle errors when finding an element in a list.

findFirst :: (a -> Bool) -> [a] -> Either String a
findFirst _ [] = Left "No element found"
findFirst p (x:xs)
    | p x       = Right x
    | otherwise = findFirst p xs

main :: IO ()
main = do
    print $ findFirst even [1,3,5,6]  
    print $ findFirst even [1,3,5]    


--HC20T8: Parser Monad for Simple Expressions
--Build a parser that parses simple expressions using the Parser monad.

import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (a,String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (x, rest) <- p input
        return (f x, rest)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (Parser pf) <*> (Parser px) = Parser $ \input -> do
        (f, rest1) <- pf input
        (x, rest2) <- px rest1
        return (f x, rest2)

instance Monad Parser where
    (Parser p) >>= f = Parser $ \input -> do
        (x, rest1) <- p input
        runParser (f x) rest1

charP :: Char -> Parser Char
charP c = Parser f
  where f (x:xs) | x == c = Just (x,xs)
                  | otherwise = Nothing
        f [] = Nothing

abParser :: Parser (Char,Char)
abParser = do
    a <- charP 'a'
    b <- charP 'b'
    return (a,b)

main :: IO ()
main = print $ runParser abParser "abc"  


--HC20T9: replicateMonad with Identity Monad
--Implement a function replicateMonad that replicates a value using the Identity monad.

import Data.Functor.Identity

replicateMonad :: Int -> a -> Identity [a]
replicateMonad n x = return (replicate n x)

main :: IO ()
main = do
    print $ runIdentity $ replicateMonad 3 "Hi"  


--HC20T10: Nested StateT and MaybeT Transformer
--Implement a nested monad transformer that combines StateT and MaybeT.

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map

type MyState = Map.Map String Int
type MyMonad = MaybeT (State MyState)

incrementKey :: String -> MyMonad Int
incrementKey key = MaybeT $ state $ \s ->
    case Map.lookup key s of
        Nothing -> (Nothing, s)
        Just v  -> (Just v, Map.insert key (v+1) s)

main :: IO ()
main = do
    let initState = Map.fromList [("a", 10), ("b", 5)]
        ((res, finalState), _) = runState (runMaybeT (incrementKey "a")) initState
    print res         
    print finalState  


--HC20T11: randomWalk with State Monad
--Write a function randomWalk using the State monad that simulates a random walk on a 2D grid.

--main.hs
module Main where

import Control.Monad.State
import System.Random
import RandomWalk

main :: IO ()
main = do
    let steps = 10          
        startPos = (0,0)     
    gen <- getStdGen
    let walk = evalState (randomWalk steps startPos) gen
    putStrLn "Random Walk positions:"
    mapM_ print walk

--RandomWalk.hs
module RandomWalk (Position, randomWalk, step) where

import Control.Monad.State
import System.Random

type Position = (Int, Int)

step :: Position -> State StdGen Position
step (x, y) = state $ \gen ->
    let (dx, gen1) = randomR (-1, 1) gen
        (dy, gen2) = randomR (-1, 1) gen1
    in ((x + dx, y + dy), gen2)

randomWalk :: Int -> Position -> State StdGen [Position]
randomWalk 0 pos = return [pos]
randomWalk n pos = do
    nextPos <- step pos
    rest <- randomWalk (n - 1) nextPos
    return (pos : rest)

--HC20T20.cabal
cabal-version: 3.8
name: HC20T11
version: 0.1.0.0
build-type: Simple

library
  hs-source-dirs: src
  exposed-modules: RandomWalk
  build-depends: base >=4.17 && <5, random, mtl
  default-language: Haskell2010

executable HC20T11-exe
  hs-source-dirs: app
  main-is: Main.hs
  build-depends: base >=4.17 && <5, random, mtl, HC20T11
  default-language: Haskell2010

--HC20T12: File Reading with IO Monad
--Use the IO monad to read a file and display its contents line-by-line.

readFileLines :: FilePath -> IO ()
readFileLines path = do
    contents <- readFile path
    mapM_ putStrLn (lines contents)


main :: IO ()
main = readFileLines "test.txt"


--HC20T13: fibonacciMemo with State Monad
--Create a function fibonacciMemo using the State monad to memoize Fibonacci calculations.

import Control.Monad.State
import qualified Data.Map as Map

type Memo = Map.Map Int Integer

fibonacciMemo :: Int -> State Memo Integer
fibonacciMemo 0 = return 0
fibonacciMemo 1 = return 1
fibonacciMemo n = do
    memo <- get
    case Map.lookup n memo of
        Just val -> return val
        Nothing -> do
            a <- fibonacciMemo (n-1)
            b <- fibonacciMemo (n-2)
            let val = a + b
            modify (Map.insert n val)
            return val

main :: IO ()
main = do
    let (result, _) = runState (fibonacciMemo 10) Map.empty
    print result   


--HC20T14: mapMFilter Monadic Map-Filter
--Implement a monadic function mapMFilter that maps and filters elements using a monad.

mapMFilter :: Monad m => (a -> m Bool) -> [a] -> m [a]
mapMFilter _ [] = return []
mapMFilter f (x:xs) = do
    keep <- f x
    rest <- mapMFilter f xs
    return $ if keep then x:rest else rest


example :: Maybe [Int]
example = mapMFilter (\x -> if x >= 0 then Just True else Nothing) [1,2,-1,3]

main :: IO ()
main = print example  


--HC20T15: treeSum with Custom Monad
--Write a function treeSum that computes the sum of elements in a binary tree using a custom monad.

import Control.Monad.Writer

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

treeSum :: Tree Int -> Writer [String] Int
treeSum (Leaf x) = do
    tell ["Leaf: " ++ show x]
    return x
treeSum (Node l r) = do
    left  <- treeSum l
    right <- treeSum r
    let s = left + right
    tell ["Node sum: " ++ show s]
    return s

exampleTree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))

main :: IO ()
main = do
    let (result, log) = runWriter (treeSum exampleTree)
    print result
    mapM_ putStrLn log


--HC20T16: retryIO with IO Monad
--Define a function retryIO that retries an IO operation up to a specified number of attempts using the IO monad.

import Control.Exception
import Control.Monad

retryIO :: Int -> IO a -> IO (Maybe a)
retryIO 0 _ = return Nothing
retryIO n action = do
    result <- try action
    case result of
        Right val -> return (Just val)
        Left  (_ :: SomeException) -> retryIO (n-1) action


main :: IO ()
main = do
    let action = readFile "nonexistent.txt"
    res <- retryIO 3 action
    print res  


--HC20T17: validatePassword with Either Monad
--Implement a function validatePassword that performs multiple checks using the Either monad to return validation errors.

validatePassword :: String -> Either [String] String
validatePassword pwd = case checks of
    [] -> Right pwd
    errs -> Left errs
  where
    checks = concat [ lengthCheck pwd
                    , upperCheck pwd
                    , digitCheck pwd ]
    lengthCheck p = [ "Password too short" | length p < 8 ]
    upperCheck p  = [ "Missing uppercase" | not (any (`elem` ['A'..'Z']) p) ]
    digitCheck p  = [ "Missing digit" | not (any (`elem` ['0'..'9']) p) ]

main :: IO ()
main = do
    print $ validatePassword "abc"       
    print $ validatePassword "Abcdef12"  


--HC20T18: MaybeT Monad Transformer for User Input
--Create a MaybeT monad transformer to combine Maybe and IO for user input validation.

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)

getPositiveInt :: MaybeT IO Int
getPositiveInt = MaybeT $ do
    putStrLn "Enter a positive number:"
    input <- getLine
    let n = reads input :: [(Int, String)]
    return $ case n of
        [(x, "")] | x > 0 -> Just x
        _ -> Nothing

main :: IO ()
main = do
    result <- runMaybeT getPositiveInt
    print result


--HC20T19: Writer Monad-based Logging System
--Implement a Writer monad-based logging system that tracks function calls and their arguments.

import Control.Monad.Writer

logFunction :: String -> Int -> Writer [String] Int
logFunction name x = writer (x*x, [name ++ " called with " ++ show x])

main :: IO ()
main = do
    let computation = do
            a <- logFunction "square" 3
            b <- logFunction "square" 4
            return (a+b)
    let (result, logs) = runWriter computation
    print result  
    mapM_ putStrLn logs


--HC20T20: batchProcessing with Monadic Bind
--Write a function batchProcessing that chains multiple monadic actions together using >>= (bind).

batchProcessing :: [Int -> IO Int] -> Int -> IO Int
batchProcessing [] acc     = return acc
batchProcessing (f:fs) acc = f acc >>= batchProcessing fs

increment :: Int -> IO Int
increment x = return (x+1)

double :: Int -> IO Int
double x = return (x*2)

main :: IO ()
main = do
    result <- batchProcessing [increment, double, increment] 3
    print result  

