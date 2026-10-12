f x = x + 1

-- mulThree: (Num a) => a -> (a -> (a -> a))
mulThree x y z = x * y * z

-- ((mulThree 2) 3) 4
mulTwo = mulThree 2 
-- mulTwo 3 4 
-- \x -> \y -> \z -> x * y * z 
-- \y -> \z -> 2 * y * z

mulSix = mulTwo 3 
-- mulSix 4
-- \z -> 2 * 3 * z

-- infix functions 
-- x + y 
-- (+) x y 
-- (2+) y
-- (+2) y
-- не камутативное
-- (/2) 4
-- (4-) 5
-- (subtract 4) 5
-- \y -> 2 + y 
-- \x -> \y -> x + y

--Функции высшего порядка принимают функцию как аргумент

applyTwice:: (a -> a) -> a -> a
applyTwice f x  = f (f x)
-- applyTwice (+3) 10 

-- map :: (a -> a) -> [a] -> [a]
-- map :: (a -> b) -> [a] -> [b] 
-- map (+3) [1,2,3,4,5,6]

-- filter even  [1,2,3,4,5]
-- filter (>5)  [1,2,3,4,5]
-- lambda function 
-- filter (\x y z -> x * y * z)  
-- filter (\x -> x > 10) [1,2,3,4,5]

-- map (*10) (filter even [1..10])
-- map (*10) (filteer odd [1..10])

-- --Свёртки
-- foldr 
-- foldl

-- foldl (+) 0 [1,2,3,4,5]
-- ghci> foldl (*) 1 [1,2,3,4,5]
-- 120

-- ghci> foldr (-) 100 [1,2,3,4,5]
-- -97

-- ghci> foldr1 (/) [1,2,3,4]
-- 0.375
-- ghci> foldl1 (/) [1,2,3,4]
-- 4.1666666666666664e-2

-- Карриование

-- Функции высшего порядка 

-- map filter foldr foldl sort reverse zip 

