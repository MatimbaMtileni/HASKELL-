--HC8T1: Create a type synonym called Address for String and a type synonym called Value for Int. Define a function generateTx :: Address -> Address -> Value -> String that takes two addresses and a value and returns a string concatenating these.

type Address = String
type Value   = Int

generateTx :: Address -> Address -> Value -> String
generateTx fromAddr toAddr amount =
  "Transaction from " ++ fromAddr ++ " to " ++ toAddr ++ " of amount " ++ show amount

main :: IO ()
main = do
  let sender = "Regent126"
  let receiver = "Valencia456"
  let amountSent = 100
  putStrLn (generateTx sender receiver amountSent)
  
  
--HC8T2: Define a new type PaymentMethod with the constructors Cash, Card, and Cryptocurrency. Create a Person type that includes a name, address (tuple of String and Int), and a payment method. Create a person bob who pays with cash.

data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person = Person {
  name :: String,
  address :: (String, Int),
  paymentMethod :: PaymentMethod
} deriving Show

bob :: Person
bob = Person {
  name = "Bob",
  address = ("Main Street", 42),
  paymentMethod = Cash
}

main :: IO ()
main = do
  putStrLn "Person info:"
  print bob
  

--HC8T3: Define a type Shape with constructors Circle Float and Rectangle Float Float. Create a function area :: Shape -> Float that calculates the area of the shape. Calculate the area of a circle with radius 5 and a rectangle with sides 10 and 5.

data Shape = Circle Float | Rectangle Float Float deriving Show

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

circle :: Shape
circle = Circle 5

rectangle :: Shape
rectangle = Rectangle 10 5

main :: IO ()
main = do
  putStrLn "Calculating areas of shapes:"
  putStrLn $ "Area of circle with radius 5: " ++ show (area circle)
  putStrLn $ "Area of rectangle 10 x 5: " ++ show (area rectangle)


--HC8T4: Define a new type Employee using record syntax with fields name :: String and experienceInYears :: Float. Create an employee richard with 7.5 years of experience.

data Employee = Employee {
  name :: String,
  experienceInYears :: Float
} deriving Show

richard :: Employee
richard = Employee {
  name = "Richard",
  experienceInYears = 7.5
}

main :: IO ()
main = do
  putStrLn "Employee information:"
  print richard
  

--HC8T5: Define a type Person using a record syntax that includes name :: String, age :: Int, and isEmployed :: Bool. Create a person1 who is employed, and a person2 who is unemployed.

data Person = Person {
  name :: String,
  age :: Int,
  isEmployed :: Bool
} deriving Show

person1 :: Person
person1 = Person {
  name = "Alice",
  age = 30,
  isEmployed = True
}

person2 :: Person
person2 = Person {
  name = "Bob",
  age = 25,
  isEmployed = False
}

main :: IO ()
main = do
  putStrLn "Person 1:"
  print person1

  putStrLn "\nPerson 2:"
  print person2
  
  
--HC8T6: Define a type Shape using record syntax with fields center :: (Float, Float), color :: String, and radius :: Float for circles, and width :: Float, height :: Float, and color :: String for rectangles. Create an instance of Shape for a circle and a rectangle.

data Shape
  = Circle {
      center :: (Float, Float),
      radius :: Float,
      circleColor :: String
    }
  | Rectangle {
      position :: (Float, Float),
      width :: Float,
      height :: Float,
      rectColor :: String
    }
  deriving Show

circle1 :: Shape
circle1 = Circle {
  center = (0.0, 0.0),
  radius = 5.0,
  circleColor = "Red"
}

rectangle1 :: Shape
rectangle1 = Rectangle {
  position = (10.0, 20.0),
  width = 15.0,
  height = 10.0,
  rectColor = "Blue"
}

main :: IO ()
main = do
  putStrLn "Circle:"
  print circle1

  putStrLn "\nRectangle:"
  print rectangle1


--HC8T7: Define a new type Animal using data with constructors Dog String and Cat String. Create a function describeAnimal :: Animal -> String that describes the animal. Create instances for a dog and a cat.

data Animal = Dog String | Cat String deriving Show

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "This is a dog named " ++ name ++ "."
describeAnimal (Cat name) = "This is a cat named " ++ name ++ "."

dog1 :: Animal
dog1 = Dog "Buddy"

cat1 :: Animal
cat1 = Cat "Whiskers"

main :: IO ()
main = do
  putStrLn $ describeAnimal dog1
  putStrLn $ describeAnimal cat1


--HC8T8: Using type synonyms, create a type synonym Name for String and a type synonym Age for Int. Define a function greet :: Name -> Age -> String that takes a name and age and returns a greeting.

type Name = String
type Age  = Int

greet :: Name -> Age -> String
greet name age = "Hello, " ++ name ++ " You are " ++ show age ++ " years old."

main :: IO ()
main = do
  let personName = "Matimba"
  let personAge  = 21
  putStrLn (greet personName personAge)


--HC8T9: Define a type Transaction with fields from :: Address, to :: Address, amount :: Value, and transactionId :: String. Define a function createTransaction :: Address -> Address -> Value -> String that creates a Transaction and returns the transaction id.

type Address = String
type Value = Int

data Transaction = Transaction {
  from :: Address,
  to :: Address,
  amount :: Value,
  transactionId :: String
} deriving Show

generateTxId :: Address -> Address -> Value -> String
generateTxId fromAddr toAddr amt = "TX-" ++ take 6 (fromAddr ++ toAddr) ++ "-" ++ show amt

createTransaction :: Address -> Address -> Value -> String
createTransaction fromAddr toAddr amt =
  let txId = generateTxId fromAddr toAddr amt
      tx = Transaction {
        from = fromAddr,
        to = toAddr,
        amount = amt,
        transactionId = txId
      }
  in transactionId tx  

main :: IO ()
main = do
  let sender = "Alice123"
  let receiver = "Bob456"
  let amountSent = 250
  let txId = createTransaction sender receiver amountSent
  putStrLn $ "Transaction created with ID: " ++ txId


--HC8T10: Using deriving Show, define a type Book with fields title :: String, author :: String, and year :: Int. Create a Book instance for a book and print it using the Show instance.

data Book = Book {
  title  :: String,
  author :: String,
  year   :: Int
} deriving Show

myBook :: Book
myBook = Book {
  title = "Learn You a Haskell for Great Good!",
  author = "Miran Lipovača",
  year = 2011
}

main :: IO ()
main = do
  putStrLn "Book information:"
  print myBook
