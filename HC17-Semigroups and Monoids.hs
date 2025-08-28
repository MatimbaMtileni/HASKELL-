--HC17T1: Severity Data Type and Semigroup Instance
--Create a Severity data type representing the severity of an emergency with four levels (Low, Medium, High, Critical).
--Implement a Semigroup instance for this type, where the higher severity overrides the lower one.

data Severity = Low | Medium | High | Critical
  deriving (Show, Eq, Ord)

instance Semigroup Severity where
  (<>) a b = max a b

main :: IO ()
main = do
  putStrLn "Severity Semigroup"
  print (Low <> Medium)     
  print (Medium <> High)    
  print (High <> Critical)  
  print (Critical <> Low)   


--HC17T2: Min and Max Newtypes with Semigroup
--Define a Min and Max newtype for any Ord type and implement their respective Semigroup instances using min and max.

newtype Min a = Min { getMin :: a } deriving (Show, Eq, Ord)
newtype Max a = Max { getMax :: a } deriving (Show, Eq, Ord)

instance Ord a => Semigroup (Min a) where
  (<>) (Min x) (Min y) = Min (min x y)

instance Ord a => Semigroup (Max a) where
  (<>) (Max x) (Max y) = Max (max x y)

main :: IO ()
main = do
  putStrLn "Min and Max Semigroups"
  print (Min 5 <> Min 3)     
  print (Max 5 <> Max 3)     
  

--HC17T3: Monoid Instance for Severity
--Implement the Monoid instance for the Severity type, where the identity value is Low.

data Severity = Low | Medium | High | Critical
  deriving (Show, Eq, Ord)

instance Semigroup Severity where
  (<>) = max

instance Monoid Severity where
  mempty = Low

main :: IO ()
main = do
  putStrLn "Severity Monoid"
  print (mempty <> Medium)  
  print (mempty <> Low)     
  print (High <> mempty)    


--HC17T4: Monoid Instance for Sum Newtype
--Implement the Monoid instance for the Sum newtype, where the identity element is 0.

newtype Sum a = Sum { getSum :: a } deriving (Show, Eq, Num)

instance Num a => Semigroup (Sum a) where
  (<>) (Sum x) (Sum y) = Sum (x + y)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

main :: IO ()
main = do
  putStrLn "Sum Monoid"
  print (Sum 3 <> Sum 4)   
  print (mempty <> Sum 5)   


--HC17T5: combineLists Function
--Implement a function combineLists that uses the Semigroup instance to concatenate two lists of integers.

combineLists :: [Int] -> [Int] -> [Int]
combineLists = (<>)

main :: IO ()
main = do
  putStrLn "combineLists"
  print (combineLists [1, 2] [3, 4])    
            

--HC17T6: maxSeverity Function
--Define a function maxSeverity that combines a list of Severity values using mconcat.

data Severity = Low | Medium | High | Critical
  deriving (Show, Eq, Ord)

instance Semigroup Severity where
  (<>) = max

instance Monoid Severity where
  mempty = Low


maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

main :: IO ()
main = do
  putStrLn "maxSeverity"
  print (maxSeverity [Low, Medium, High])           
  print (maxSeverity [Medium, Medium, Low])         
  print (maxSeverity [Low, Low, Low])               
  print (maxSeverity [High, Critical, Medium])      


--HC17T7: multiplyProducts Function
--Implement a function multiplyProducts that takes a list of Product values and returns the combined result using mconcat.

newtype Product a = Product { getProduct :: a }
  deriving (Show, Eq, Num)

instance Num a => Semigroup (Product a) where
  (<>) (Product x) (Product y) = Product (x * y)

instance Num a => Monoid (Product a) where
  mempty = Product 1

multiplyProducts :: Num a => [Product a] -> Product a
multiplyProducts = mconcat

main :: IO ()
main = do
  putStrLn "multiplyProducts"
  print (multiplyProducts [Product 2, Product 3, Product 4]) 
 

--HC17T8: foldWithSemigroup Function
--Write a function foldWithSemigroup that accepts any list of a type with a Semigroup instance and combines all elements using foldr.

newtype Sum a = Sum { getSum :: a } deriving (Show, Eq, Num)

instance Num a => Semigroup (Sum a) where
  (<>) (Sum x) (Sum y) = Sum (x + y)

foldWithSemigroup :: Semigroup a => [a] -> a
foldWithSemigroup = foldr1 (<>)

main :: IO ()
main = do
  putStrLn "foldWithSemigroup"
  print (foldWithSemigroup ["Hello", " ", "World"]) 
  print (foldWithSemigroup [[1, 2], [3], [4, 5]])   
  print (foldWithSemigroup [Sum 1, Sum 2, Sum 3])


--HC17T9: Config Data Type and Semigroup Instance
--Define a Config data type with fields for loggingLevel, timeout, and retries.
--Implement a Semigroup instance that combines configurations by taking the maximum loggingLevel and retries and the minimum timeout.

data Config = Config
  { loggingLevel :: Int
  , timeout      :: Int
  , retries      :: Int
  } deriving (Show)

instance Semigroup Config where
  (<>) (Config l1 t1 r1) (Config l2 t2 r2) =
    Config (max l1 l2) (min t1 t2) (max r1 r2)

main :: IO ()
main = do
  putStrLn "Config Semigroup"
  let c1 = Config 2 100 3
  let c2 = Config 4 80 5
  print (c1 <> c2) 


--HC17T10: Monoid Instance for Config
--Implement the Monoid instance for the Config data type, where the identity element is a configuration with the lowest loggingLevel, highest timeout, and lowest retries.

data Config = Config
  { loggingLevel :: Int
  , timeout      :: Int
  , retries      :: Int
  } deriving (Show)

instance Semigroup Config where
  (<>) (Config l1 t1 r1) (Config l2 t2 r2) =
    Config (max l1 l2) (min t1 t2) (max r1 r2)

instance Monoid Config where
  mempty = Config 0 maxBound 0

main :: IO ()
main = do
  putStrLn "Config Monoid"
  let c1 = Config 2 100 3
  let c2 = Config 4 80 5
  print (mempty <> c1) 
  print (c1 <> mempty) 
  print (c1 <> c2)     
