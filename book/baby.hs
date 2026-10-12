doubleMe x = x + x 
-- for use ghci> :l baby ghci> doubleMe 9 -> 18


factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- doubleUs x y = x*2 + y*2 
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

doubleSmallNum :: (Ord a, Num a) => a -> a
doubleSmallNum x = (if x > 100 then x else x * 2) + 1


-- ghci> let lostNUmbers = [4, 8, 15, 16, 23, 42] 
-- ghci> lostNUmbers 
-- [4,8,15,16,23,42]
-- ghci> map (^2) lostNUmbers
-- [16,64,225,256,529,1764]
-- ghci> [1,2,3,4,5,6] ++ "Hello"
-- ghci> [1,2,3,4,5,6] ++ ["H"]  
-- ghci> [1,2,3,4,5,6] ++ [1,2,3,4,5] 
-- [1,2,3,4,5,6,1,2,3,4,5]
-- ghci> [1,2,3,4] : 5
-- ghci> 5:[1,2,3,4]  
-- [5,1,2,3,4]

-- ghci> 1:2:3:[]
-- [1,2,3]
-- ghci> [1,2,3,4,5]!2

-- ghci> [1,2,3,4,5] !! 0
-- 1
-- ghci> [1,2,3,4,5] !! 1
-- 2

-- ghci> [1,2,3,4,5] !! 2 
-- 3
-- ghci> [1,2,3,4,5] !! 3
-- 4
-- ghci> [1,2,3,4,5] !! 4
-- 5
-- ghci> [1,2,3] < [0, 1, 2]
-- False
-- ghci> [1,2,3] > [0, 1, 2]
-- True
-- ghci> head [1,2,3] 
-- 1
-- ghci> [1,2,3] tail
-- ghci> tail [1,2,3]
-- [2,3]
-- ghci> last [1,2,3]
-- 3
-- ghci> take 1 [1,2]
-- [1]
-- ghci> drop 1 [1,2]
-- [2]

minimum макс
maximum мин 
sum сумма
product произведение
elem проверка вхождения
4 `elem` [1,2,3,4]
True
cycle принимает список и зацикливает его в бесконечный. Если вы попробуете отобразить результат, на это уйдёт целая вечность, поэтому вам придётся где-то его обрезать
ghci> take 11 (cycle "zxc ")
"zxc zxc zxc"
repeat принимает элемент и возвращает бесконечный список, состоящий только из этого элемента. 
ghci> take 11 (repeat "zxc") 
["zxc","zxc","zxc","zxc","zxc","zxc","zxc","zxc","zxc","zxc","zxc"]
replicate 3 10 вернёт [10,10,10]
ghci> [x*2 | x <- [1..10]] генератор
[2,4,6,8,10,12,14,16,18,20]
ghci> [x*2 | x <- [1..10], x*2 > 10] генератор с условием
[12,14,16,18,20]
ghci> [x | x <- [50..100], x `mod` 7 == 3]
[52,59,66,73,80,87,94]