import Data.List (nub)
-- 1 произайдет ошибка так как функция не рассматривает такой случай 
classify :: Int -> String 
classify x 
    | x > 0 = "pos"
    | x == 0 = "zero"
    | x < 0 = "neg"

binnom :: Integer -> Integer -> Integer 
binnom n k  
    | k == 0 = 1 
    | k == n = 1 
    | k < 0 || k > n = 0
    | otherwise = binnom (n-1) (k-1) + binnom (n-1) k 

tokenize:: String -> [String]
tokenize [] = []
tokenize s = reverse (tokenize' s "" [])  
    where 
        tokenize' [] acc res
            | null acc = res -- если строка кончилась 
            | otherwise = (reverse acc:res) -- если строка кончилась 
        
        tokenize' (x:xs) acc res -- acc копит текущую строку 
            | x == ' '  = 
                if null acc 
                    then tokenize' xs "" res 
                    else tokenize' xs "" (reverse acc : res) 
            | otherwise = tokenize' xs (x:acc) res 

--4 + 1

countEven :: [Int] -> Int 
countEven arr = foldl (\acc x -> if even x then acc+1 else acc) 0 arr

sumPositive :: [Int] -> Int 
sumPositive arr = foldl (\acc x -> if x > 0 then acc+x else acc) 0 arr 


listStats :: [Int] -> Maybe (Int, Int, Int)
listStats arr 
    | null arr  = Nothing 
    | otherwise = Just (countPositive arr, sumNegative arr , countUnique arr)
    where 
        countPositive arr = foldl (\acc x -> if x > 0 then acc+1 else acc) 0 arr
        sumNegative arr = foldl (\acc x -> if x < 0 then acc+x else acc) 0 arr
        countUnique arr = length (nub arr)

