--HC9T1: Define a Parametric Type Synonym
--Create a parametric type synonym called Entity a to represent various types of entities with addresses.

type Entity a = (a, String)

data Person = Person { name :: String, age :: Int } deriving Show
data Company = Company { companyName :: String, registrationNumber :: Int } deriving Show

person1 :: Entity Person
person1 = (Person "Alice" 30, "555 Main St")

company1 :: Entity Company
company1 = (Company "Coxygen" 584264, "555 cardano Rd")

main :: IO ()
main = do
  putStrLn "Person Entity:"
  print person1
  putStrLn "Company Entity:"
  print company1
  

--HC9T2: Implement a Parametric Data Type
--Create a data type Box a with two constructors, Empty and Has a, to represent a box that may or may not contain a value.

data Box a = Empty | Has a deriving Show

box1 :: Box Int
box1 = Has 42

box2 :: Box String
box2 = Has "Hello"

box3 :: Box Double
box3 = Empty

describeBox :: Show a => Box a -> String
describeBox Empty    = "The box is empty."
describeBox (Has x)  = "The box contains: " ++ show x

main :: IO ()
main = do
  putStrLn $ describeBox box1
  putStrLn $ describeBox box2
  putStrLn $ describeBox box3


--HC9T3: Function to Add Values in a Box
--Create a function addN that takes a number and a Box a. If the box contains a number, add the given number to it.

data Box a = Empty | Has a deriving Show

addN :: Num a => a -> Box a -> Box a
addN _ Empty   = Empty
addN n (Has x) = Has (x + n)

box1 :: Box Int
box1 = Has 10

box2 :: Box Int
box2 = Empty

main :: IO ()
main = do
  putStrLn "Original box1:"
  print box1
  putStrLn "After adding 5:"
  print (addN 5 box1)

  putStrLn "\nOriginal box2 (empty):"
  print box2
  putStrLn "After adding 5:"
  print (addN 5 box2)
  
  
--HC9T4: Extract a Value from a Box
--Create a function extract that takes a default value and a Box a. It returns the value inside the box or the default if the box is empty.

data Box a = Empty | Has a deriving Show

extract :: a -> Box a -> a
extract def Empty   = def
extract _   (Has x) = x

box1 :: Box String
box1 = Has "Hello"

box2 :: Box String
box2 = Empty

main :: IO ()
main = do
  putStrLn "Extracting from box1:"
  putStrLn $ extract "Default" box1

  putStrLn "\nExtracting from box2 (empty):"
  putStrLn $ extract "Default" box2
  
  
--HC9T5: Parametric Data Type with Record Syntax
--Define a parametric data type Shape a with constructors Circle and Rectangle, both containing a color field of type a.

data Shape a
  = Circle { radius :: Float, centerX :: Float, centerY :: Float, color :: a }
  | Rectangle { width :: Float, height :: Float, posX :: Float, posY :: Float, color :: a }
  deriving Show

circle1 :: Shape String
circle1 = Circle { radius = 5.0, centerX = 0.0, centerY = 0.0, color = "Red" }

rect1 :: Shape String
rect1 = Rectangle { width = 10.0, height = 4.0, posX = 1.0, posY = 2.0, color = "Blue" }

Color :: Shape a -> a
Color = color

main :: IO ()
main = do
  print circle1
  print rect1
  putStrLn $ "Circle color: " ++ Color circle1
  putStrLn $ "Rectangle color: " ++ Color rect1
  

--HC9T6: Recursive Data Type for Tweets
--Define a recursive data type Tweet that represents a tweet with content, likes, and comments, which are themselves tweets.

data Tweet = Tweet
  { content  :: String
  , likes    :: Int
  , comments :: [Tweet] 
  } deriving Show

reply1 :: Tweet
reply1 = Tweet
  { content = "Nice post!"
  , likes = 3
  , comments = []
  }

reply2 :: Tweet
reply2 = Tweet
  { content = "I disagree."
  , likes = 1
  , comments = []
  }

mainTweet :: Tweet
mainTweet = Tweet
  { content = "Just posted my Haskell project!"
  , likes = 15
  , comments = [reply1, reply2]
  }

printTweet :: Tweet -> Int -> IO ()
printTweet (Tweet content likes comments) indent = do
  putStrLn $ replicate indent ' ' ++ "- " ++ content ++ " (" ++ show likes ++ " likes)"
  mapM_ (\c -> printTweet c (indent + 2)) comments

main :: IO ()
main = do
  putStrLn "Tweet thread:"
  printTweet mainTweet 0


--HC9T7: Engagement Function for Tweets
--Create a function engagement that calculates the engagement of a Tweet by summing its likes and the engagement of its comments.

data Tweet = Tweet
  { content  :: String
  , likes    :: Int
  , comments :: [Tweet]
  } deriving Show

engagement :: Tweet -> Int
engagement (Tweet _ likes comments) =
  likes + sum (map engagement comments)

reply1 :: Tweet
reply1 = Tweet
  { content = "Interesting"
  , likes = 2
  , comments = []
  }

reply2 :: Tweet
reply2 = Tweet
  { content = "Thanks for sharing"
  , likes = 4
  , comments = []
  }

nestedReply :: Tweet
nestedReply = Tweet
  { content = "I agree with this comment"
  , likes = 1
  , comments = [reply2]
  }

mainTweet :: Tweet
mainTweet = Tweet
  { content = "Haskell is good to study"
  , likes = 10
  , comments = [reply1, nestedReply]
  }

main :: IO ()
main = do
  putStrLn $ "Total engagement: " ++ show (engagement mainTweet)
  
  
-- HC9T8: Recursive Sequence Data Type
-- Define a recursive data type Sequence a representing a linear sequence of nodes, each containing a value and pointing to the next node.

data Sequence a
  = End                        
  | Node a (Sequence a)        
  deriving Show

-- Example sequence
seq1 :: Sequence Int
seq1 = Node 1 (Node 2 (Node 3 End))

-- Convert sequence to list
toList :: Sequence a -> [a]
toList End         = []
toList (Node x xs) = x : toList xs

-- Compute the length of a sequence
seqLength :: Sequence a -> Int
seqLength End         = 0
seqLength (Node _ xs) = 1 + seqLength xs

-- Main program
main :: IO ()
main = do
  putStrLn "Sequence as list:"
  print $ toList seq1

  putStrLn "Length of sequence:"
  print $ seqLength seq1

  
  
--HC9T9: Check for Element in a Sequence
--Create a function elemSeq that checks if a given element is inside a Sequence.

data Sequence a
  = End
  | Node a (Sequence a)
  deriving (Show, Eq) 

elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node y ys)
  | x == y    = True
  | otherwise = elemSeq x ys

Seq :: Sequence Int
Seq = Node 1 (Node 2 (Node 3 End))

main :: IO ()
main = do
  putStrLn "Checking for 2 in sequence:"
  print $ elemSeq 2 Seq   

  putStrLn "Checking for 5 in sequence:"
  print $ elemSeq 5 Seq
  
  
--HC9T10: Binary Search Tree Data Type
--Define a binary search tree type BST a with constructors for an empty tree and a node containing a value and two subtrees.

data BST a
  = Empty                         
  | Node a (BST a) (BST a)        
  deriving (Show, Eq)

Tree :: BST Int
Tree =
  Node 10
    (Node 5
      (Node 2 Empty Empty)
      (Node 7 Empty Empty)
    )
    (Node 15
      Empty
      (Node 20 Empty Empty)
    )

inOrder :: BST a -> [a]
inOrder Empty = []
inOrder (Node val left right) =
  inOrder left ++ [val] ++ inOrder right

main :: IO ()
main = do
  putStrLn "In-order traversal of exampleTree:"
  print $ inOrder Tree