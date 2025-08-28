--HC18T1: mapToLower Function with fmap
--Define a function mapToLower using fmap that converts all characters in a list to lowercase.

import Data.Char (toLower)

mapToLower :: String -> String
mapToLower = fmap toLower

main :: IO ()
main = do
    print (mapToLower "Hello HASKELL")


--HC18T2: Functor Instance for Tree
--Create a Functor instance for the binary tree type Tree.

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

exampleTree :: Tree Int
exampleTree = Node 10 (Node 5 Empty Empty) (Node 15 Empty Empty)

main :: IO ()
main = do
    print (fmap (*2) exampleTree)


--HC18T3: incrementTreeValues Function
--Define a function incrementTreeValues that adds one to every value in a tree using the Functor instance.

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

incrementTreeValues :: Num a => Tree a -> Tree a
incrementTreeValues = fmap (+1)

exampleTree :: Tree Int
exampleTree = Node 10 (Node 5 Empty Empty) (Node 15 Empty Empty)

main :: IO ()
main = do
    print (incrementTreeValues exampleTree)


--HC18T4: mapToBits Function
--Implement a function mapToBits to convert a list of Booleans to characters '1' or '0' using fmap.

mapToBits :: [Bool] -> [Char]
mapToBits = fmap (\b -> if b then '1' else '0')

main :: IO ()
main = do
    print (mapToBits [True, False, True, True, False])


--HC18T5: Functor Instance for Either
--Define a Functor instance for the Either type, applying fmap only to the Right case.

main :: IO ()
main = do
    print (fmap (*3) (Right 4 :: Either String Int))
    print (fmap (*3) (Left "Error" :: Either String Int))


--HC18T6: applyToMaybe Function
--Implement a function applyToMaybe that uses fmap to transform the value inside a Maybe.

applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap

main :: IO ()
main = do
    putStrLn "HC18T6: applyToMaybe"
    print (applyToMaybe (+1) (Just 5))
    print (applyToMaybe (+1) Nothing)


--HC18T7: fmapTuple Function
--Define a function fmapTuple that applies a function to the second element of a tuple (a, b) using the Functor instance of (,) a.

fmapTuple :: (b -> c) -> (a, b) -> (a, c)
fmapTuple = fmap

main :: IO ()
main = do
    print (fmapTuple (++ ".") ("Hello", "Matimba"))


--HC18T8: identityLawCheck Function
--Implement the identityLawCheck function to verify the Functor identity law.

identityLawCheck :: (Functor f, Eq (f a)) => f a -> Bool
identityLawCheck x = fmap id x == x

main :: IO ()
main = do
    print (identityLawCheck (Just 5))
    print (identityLawCheck [1, 2, 3])


--HC18T9: compositionLawCheck Function
--Implement the compositionLawCheck function to verify the Functor composition law.


compositionLawCheck :: (Functor f, Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
compositionLawCheck f g x = fmap (f . g) x == (fmap f . fmap g) x

main :: IO ()
main = do
    print (compositionLawCheck (*2) (+3) (Just 4))
    print (compositionLawCheck (++ "!") reverse ["abc", "def"])


--HC18T10: nestedFmap Function
--Create a function nestedFmap that applies a function to a nested structure using multiple fmap calls.

nestedFmap :: (a -> b) -> [[a]] -> [[b]]
nestedFmap = fmap . fmap

main :: IO ()
main = do
    print (nestedFmap (*2) [[1,2], [3,4], []])
