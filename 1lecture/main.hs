-- Hello World
main :: IO ()
main = putStrLn "Привет, Haskell!"

-- doubleMe input int output int
doubleMe :: Int -> Int
doubleMe x = x + x
-- func with two arguments
sumUs :: Int -> Int -> Int
sumUs x y = x + y

-- else is required

doubleSmallNumber x = if x > 100 
    then x 
    else x * 2

-- pattern matching 
factorial :: Integer -> Integer
factorial 0 = 1 -- instend if n == 0 then 1 else 
factorial n = n * factorial (n - 1)

evensTimesTwo = [x * 2 | x <- [1..10], even x]


