{-Задание 0-}
{-
    Массив для заданий 1-4
-}
import Data.Array 

arr:: Array (Int, Int) Int 
arr = array ((1,1), (2,3)) [
    ((1,1),8), ((1,2),11), ((1,3),4),
    ((2,1),3), ((2,2),2),  ((2,3),4)]

{-Задание 1-}
{-
    Разные способы сумировать элементы массива 
    используя elems foldl1 sum
-}
sum1 = foldl (+) 0 (elems arr)
sum2 = foldl1 (+) arr
sum3 = sum arr 

{-Задание 2-}
{-
    Суммирует элементы массива полученные черзе доступ по индексам
-}
sum4 = foldl (+) 0 [arr ! ind | ind <- (indices arr)]

{-Задание 3-}
{-
    Список элементов arr ! ind где сумма индексов равна значению (fst ind + snd ind) == (arr ! ind)
-}
elementIsIdx = [arr ! ind | ind <- (indices arr), (fst ind + snd ind) == (arr ! ind)] 

{-Задание 4-}
{-
    Заменяем элементы где сумма индексов равна значению на 77
    используя инкрементальное обновление через //
-}
replaceTo77 = arr // [(ind, 77) | ind <- (indices arr), (fst ind + snd ind) == (arr ! ind)]

{-Задание 5-}
{-
    Путь ломанной определяется как сумма растояний соседних точек
    С помощью рекурсии складываем растояния всех соседних точек 
    в заданном массиве.
-}
arr2 :: Array Int (Double, Double)  
arr2 = listArray (1,5) ([((-5.23),4.2),(2.456,13.234),(11241,211.2),((-1),0),(8.3,3.8)])

lengthOfPolygonal :: Array Int (Double, Double) -> Double
lengthOfPolygonal arr = helper (elems arr)
    where 
        distance dot1 dot2 = sqrt ((fst dot2 - fst dot1)^2 + (snd dot2 - snd dot1)^2)
        helper [] = 0 
        helper [_] = 0
        helper (a:b:xs) = distance a b + helper (b:xs)

test5Solution:: IO()
test5Solution = do 
    print (lengthOfPolygonal arr2)
{-Результат-}
{-
ghci> test5Solution         
22506.178720838434
-}

{-Задание 6-}
{-
    Выводит массив усредненных значений по соседним величинам
    Усредненная координата точки считается, как (сосед слева + точка + сосед справа) / 3
    для крайних точек в начале (точка + сосед справа) / 2
    для крайних точек в конце (сосед слева + точка) / 2
    
    b!i = (a!(i-1) + a!i + a!(i+1))/3
-}

a :: Array Int Double
a = listArray (1,10) ([1.3,1.2,1.9,1.25,1.4,0.1,1.2,1.32,1.23,1.5])

middleArray :: Array Int Double -> Array Int Double
middleArray arr = listArray (bounds arr) (helper (elems arr) 0)
    where
        helper [] _ = []
        helper [x] _ = [x]
        helper [x, y] _ = [(x + y) / 2]
        helper (a:b:c:xs) idx 
            | idx == 0  = (a + b) / 2 : helper (a:b:c:xs) (idx+1) 
            | otherwise = (a + b + c) / 3 : helper (b:c:xs) (idx+1)

test6Solution:: IO()
test6Solution = do 
    print (middleArray a)

{-Результат-}
{-
ghci> test6Solution 
array (1,10) [(1,1.25),(2,1.4666666666666668),(3,1.45),(4,1.5166666666666666),(5,0.9166666666666666),(6,0.9),(7,0.8733333333333334),(8,1.25),(9,1.3499999999999999),(10,1.365)]
-}



