func :: Int -> Int -> Int 
func x y = x + y

avg :: [Int] -> Double 
avg l = fromIntegral (sum l) / fromIntegral (length l)

-- Pattern Matching 

func123:: Int -> String
-- func123 x = if x == 67 
--             then "Ok" 
--             else "Wrong"

func123 67 = "Ok"
func123 _ = "Wrong"


-- if (x == 7) ...
-- elif (x==25) .. 
-- elif (x ==3)
 
-- Match Case 
-- match (x):
--     case 7:
--     case 35:
--     case 67:


fact :: (Integral a) => a -> a
fact 0 = 1
fact 1 = 1
fact n = fact (n - 1) * n

fibb :: Int -> Int
fibb 0 = 1
fibb 1 = 1
fibb n = fibb (n - 2) + fibb (n - 1)
-- 1 1 2 3 5 8

-- Guards
-- max' x y = if (x > y) then x else y
max' :: (Ord a, Num a) => a -> a -> a
max' x y
    |x > y     = x
    |x == y    = x
    |otherwise = y

isEmptyList :: [a] -> Bool
isEmptyList x = case x of
    [] -> True
    _  -> False

bmi :: Double -> Double -> String 
bmi weight height 
    | bmiValue < 18 = "дрыщ ебаный"
    | bmiValue < 25 = "пузатый"
    | bmiValue < 30 = "жиробас"
    | otherwise = "слон"
    where bmiValue = weight/(height^2)

cylinder :: Double -> Double -> Double 
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in sideArea + topArea*2

-- if let x = 7 in x + 2 > 10 then ...

-- a, b, c = 10, 20, 30

-- Tail Recursion

length' :: [a] -> Int
length' [] = 0
length' (  x  : xs ) = 1 + length' xs 
        -- head : tail
-- [1,2,3]
-- sum 1 + 2 + 3
-- 1:[2,3]
--     2: [3]
--         3: []
sum' :: (Num a) => [a] -> a 
sum' [] = 0 
sum' (x:xs) = x + sum' xs