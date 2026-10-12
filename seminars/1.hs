-- main :: IO()

-- main = do 
--     name <- getLine
--     putStrLn ("Hello, " ++ name)

-- double x = x + x
-- doubleUs x y = double x + double y

-- factorial :: (Eq a, Num a) => a -> a
-- factorial 0 = 1
-- factorial x = factorial (x - 1) * x

-- fibanacci :: (Eq a, Num a) => a -> a
-- fibanacci 1 = 1
-- fibanacci 2 = 1
-- fibanacci x = fibanacci (x - 1) + fibanacci(x - 2)
-- -- 1 1 2 3 5 8 13

-- doubleSmall x = if x > 100 
--                     then x 
--                     else x * 2

-- func1 x = (x ** 2) / (1 + x)
-- func2 x = sqrt(3 * x - x^3)
-- func3 x = logBase 10 (x^2 - 4)
-- func4 x = sqrt(sin (2 * x)) - sqrt(sin (3 * x)) 
-- func5 x = logBase 10 (1 - 2 * cos x)

-- chisl n = n * n - 1
-- znam n = 2 * n * n - n - 1
-- func n = (chisl n)/(znam n)

chisl2 n = sum([(2*x - 1)^2 | x <- [1..n]])
znam2 n = sum([(2*x)^2 | x <- [1..n]])
func431 n = (chisl2 n)/(znam2 n)




-- -- [1,2,3] !! 0 = 1 

-- -- Функция уменьшает число, пока оно не дойдет до 0 (или ниже)
-- minusSeven x = x - 7
-- countdown :: Int -> IO ()
-- countdown x
--     | x <= 0 = putStrLn "Достигли нуля!" -- Условие выхода
--     | otherwise = do
--         print x
--         countdown (minusSeven x) -- Рекурсивный вызов

-- main :: IO ()
-- main = countdown 1000 -- Выведет: 21, затем 14, затем 7, затем "Достигли нуля!"
-- -- [x*x | x <- [1..10], x*x > 50]

