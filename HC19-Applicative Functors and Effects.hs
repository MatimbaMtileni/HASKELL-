--HC19T1: Applicative Instance for Pair
--Define an Applicative instance for a custom data type Pair a.

data Pair a = Pair a a deriving (Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

main :: IO ()
main = do
    print $ (Pair (+1) (*2)) <*> (Pair 10 20)
    print $ pure 5 :: Pair Int


--HC19T2: addThreeApplicative Function
--Implement a function addThreeApplicative that adds three Maybe Int values using applicative style.

import Control.Applicative (liftA3)

addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative = liftA3 (\x y z -> x + y + z)

main :: IO ()
main = do
    print $ addThreeApplicative (Just 2) (Just 3) (Just 5)
    print $ addThreeApplicative (Just 2) Nothing (Just 5)


--HC19T3: safeProduct for Maybe Int
--Define a function safeProduct that calculates the product of a list of Maybe Int values, returning Nothing if any value is Nothing.

safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = fmap product . sequenceA

main :: IO ()
main = do
    print $ safeProduct [Just 2, Just 3, Just 4]  
    print $ safeProduct [Just 2, Nothing, Just 4] 


--HC19T4: liftAndMultiply with liftA2
--Create a function liftAndMultiply that lifts a binary function (Int -> Int -> Int) using liftA2.

import Control.Applicative (liftA2)

liftAndMultiply :: Int -> Int -> Int
liftAndMultiply = (*)

main :: IO ()
main = do
    print $ liftA2 liftAndMultiply (Just 3) (Just 4) 
    print $ liftA2 liftAndMultiply (Just 3) Nothing


--HC19T5: applyEffects with <*>
--Implement the function applyEffects using <*>, where it takes a tuple (IO Int, IO Int) and sums the values while printing them.

applyEffects :: (IO Int, IO Int) -> IO Int
applyEffects (io1, io2) = (+) <$> io1 <*> io2

readInt :: String -> IO Int
readInt prompt = do
    putStrLn prompt
    readLn

main :: IO ()
main = do
    result <- applyEffects (readInt "Enter first number: ", readInt "Enter second number: ")
    putStrLn $ "Sum: " ++ show result


--HC19T6: repeatEffect with forever
--Define the function repeatEffect that repeatedly executes an effect using forever.

import Control.Monad (forever)

repeatEffect :: IO () -> IO ()
repeatEffect action = forever action


--HC19T7: conditionalPrint with when
--Write a function conditionalPrint using when that prints a message only when a given condition is true.

import Control.Monad (when)

conditionalPrint :: Bool -> String -> IO ()
conditionalPrint cond msg = when cond (putStrLn msg)

main :: IO ()
main = do
    conditionalPrint True "Condition met"
    conditionalPrint False "This will not print."


--HC19T8: discardSecond with <*
--Implement a custom function discardSecond that uses the <* operator to return the first argument after sequencing effects.

discardSecond :: Applicative f => f a -> f b -> f a
discardSecond = (<*)

main :: IO ()
main = do
    result <- discardSecond (putStrLn "First effect" >> return 10)
                            (putStrLn "Second effect" >> return 20)
    print result


--HC19T9: pureAndApply Demonstration
--Implement pureAndApply which demonstrates how pure works with applicative effects.


pureAndApply :: IO ()
pureAndApply = do
    print $ pure (+3) <*> Just 7  
    print $ pure (*2) <*> Just 10 


--HC19T10: combineResults for Either
--Create a function combineResults that combines two Either values using applicative style.

import Control.Applicative (liftA2)

combineResults :: (Num a) => Either String a -> Either String a -> Either String a
combineResults = liftA2 (+)

main :: IO ()
main = do
    print $ combineResults (Right 3) (Right 4)          
    print $ combineResults (Right 3) (Left "Error")     

