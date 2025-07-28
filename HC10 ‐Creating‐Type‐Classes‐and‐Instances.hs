--HC10T1: ShowSimple Type Class
--Define a new type class ShowSimple that requires a function showSimple :: a -> String for simple string conversion.
--Implement an instance for PaymentMethod type.

data PaymentMethod = Cash | Card | CC

class ShowSimple a where
  showSimple :: a -> String

instance ShowSimple PaymentMethod where
  showSimple Cash = "Cash"
  showSimple Card = "Card"
  showSimple CC   = "Credit Card"


main :: IO ()
main = do
  let payment1 = Cash
  let payment2 = Card
  let payment3 = CC
  putStrLn $ "Payment 1: " ++ showSimple payment1
  putStrLn $ "Payment 2: " ++ showSimple payment2
  putStrLn $ "Payment 3: " ++ showSimple payment3


--HC10T2: Summable Type Class
--Create a type class Summable that provides sumUp :: [a] -> a.
--Implement it for the type Int.


class Summable a where
  sumUp :: [a] -> a

instance Summable Int where
  sumUp = sum

main :: IO ()
main = do
  let numbers :: [Int]
      numbers = [1, 2, 3, 4, 5]
  putStrLn $ "Sum of numbers: " ++ show (sumUp numbers)


--HC10T3: Comparable Type Class
--Define a type class Comparable with a function compareWith :: a -> a -> Ordering.
--Implement it for Blockchain.

data Blockchain = Blockchain
  { name :: String
  , popularity :: Int
  } deriving Show

class Comparable a where
  compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
  compareWith b1 b2 = compare (popularity b1) (popularity b2)

orderingToStr :: Ordering -> String
orderingToStr LT = "Less than"
orderingToStr EQ = "Equal"
orderingToStr GT = "Greater than"

main :: IO ()
main = do
  let btc = Blockchain "Bitcoin" 95
  let eth = Blockchain "Ethereum" 90
  let doge = Blockchain "Dogecoin" 95

  putStrLn $ "BTC vs ETH: " ++ orderingToStr (compareWith btc eth)
  putStrLn $ "BTC vs DOGE: " ++ orderingToStr (compareWith btc doge)

--HC10T4: Eq Instance for Box
--Create a parameterized type Box a and make it an instance of Eq.

data Box a = Box a
  deriving Show  

instance Eq a => Eq (Box a) where
  (Box x) == (Box y) = x == y

main :: IO ()
main = do
  let box1 = Box 42
  let box2 = Box 42
  let box3 = Box 99

  print (box1 == box2)  
  print (box1 == box3) 

--HC10T5: ShowDetailed Type Class
--Define a type class ShowDetailed with a function showDetailed :: a -> String.
--Implement it for a type User.

data User = User
  { username :: String
  , age :: Int
  , email :: String
  }

class ShowDetailed a where
  showDetailed :: a -> String

instance ShowDetailed User where
  showDetailed (User uname age email) =
    "User Details:\n"
    ++ "- Username: " ++ uname ++ "\n"
    ++ "- Age: " ++ show age ++ "\n"
    ++ "- Email: " ++ email

main :: IO ()
main = do
  let u1 = User "matimba mtileni" 25 "matimbamtileni07@gmail.com"
  putStrLn (showDetailed u1)



--HC10T6: Mutual Recursion in Eq for Blockchain
--Modify the Eq type class to use mutual recursion between == and /= in an instance for the Blockchain type.

data Blockchain = Block Int Blockchain | Genesis
  deriving Show

instance Eq Blockchain where
  Genesis == Genesis = True
  Block x1 r1 == Block x2 r2 = x1 == x2 && r1 == r2
  _ == _ = False

  a /= b = not (a == b)

main :: IO ()
main = do
  let chain1 = Block 10 (Block 20 Genesis)
  let chain2 = Block 10 (Block 20 Genesis)
  let chain3 = Block 10 (Block 30 Genesis)

  print (chain1 == chain2) 
  print (chain1 == chain3) 
  print (chain1 /= chain3) 
  

--HC10T7: Convertible Type Class
--Create a type class Convertible with convert :: a -> b.
--Implement it for converting PaymentMethod to String.

data PaymentMethod = Cash | CreditCard | Crypto
  deriving Show

class Convertible a b where
  convert :: a -> b

instance Convertible PaymentMethod String where
  convert Cash       = "Paid with cash"
  convert CreditCard = "Paid with credit card"
  convert Crypto     = "Paid with cryptocurrency"

main :: IO ()
main = do
  let pm1 = Cash
  let pm2 = Crypto
  putStrLn $ convert pm1  
  putStrLn $ convert pm2  




--HC10T8: AdvancedEq Subclass of Eq
--Define a subclass AdvancedEq of Eq with an additional method compareEquality :: a -> a -> Bool.

class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> Bool

data User = User { name :: String, age :: Int }

instance Eq User where
  (User name1 age1) == (User name2 age2) = name1 == name2 && age1 == age2

instance AdvancedEq User where
  compareEquality u1 u2 = u1 == u2  

main :: IO ()
main = do
  let user1 = User "Alice" 25
  let user2 = User "Alice" 25
  let user3 = User "Bob" 30

  print (compareEquality user1 user2) 
  print (compareEquality user1 user3) 



--HC10T9: MinMax Type Class
--Implement a type class MinMax with methods minValue :: a and maxValue :: a, and provide instances for Int.

class MinMax a where
  minValue :: a
  maxValue :: a

instance MinMax Int where
  minValue = minBound
  maxValue = maxBound

main :: IO ()
main = do
  putStrLn ("Min Int: " ++ show (minValue :: Int))
  putStrLn ("Max Int: " ++ show (maxValue :: Int))


--HC10T10: Concatenatable Type Class
--Create a type class Concatenatable with a function concatWith :: a -> a -> a.
--Implement it for the type [Char] (String).

class Concatenatable a where
  concatWith :: a -> a -> a

instance Concatenatable [Char] where
  concatWith x y = x ++ y

main :: IO ()
main = do
  let part1 = "Hello, "
  let part2 = "world."
  let result = concatWith part1 part2
  putStrLn ("Concatenated result: " ++ result)
