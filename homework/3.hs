{-ЗАДАНИЕ 1-}
{-
    Проверяем что все элементы равны
    прерывается на первом же несоответствии благодаря ленивому оператору &&
    
    Пример: [1,1,1,1]
    1 шаг x = 1 y = 1 xs = [1,1]
    1 == 1 && 
            2шаг x = y = 1 y = 1 xs = [1]
            1 == 1 && 
                    3 шаг [1] allEqual [_] = True 
    
-}
allEqual:: (Eq a) => [a] -> Bool 
allEqual [] = True
allEqual [_] = True
allEqual (x:y:xs) = (x == y) && allEqual (y:xs) 
{-Проверка-}
test1Solution :: IO ()
test1Solution = do
    print (allEqual [1,1,1,1,1])
    print (allEqual ["hello", "hello", ['h','e','l','l','o']])
    print (allEqual ['2','2','2','2'])
{-
Результат:
    ghci> test1Solution 
    True
    True
    True
-}

{-ЗАДАНИЕ 2-}
{-
    Список пифагоровых троек на натуральных числах, 
    используя условие x^2 + y^2 = z^2
    
    Так как список должнен быть полными без повторов и бесконечными
    будем брать 
    z >= 1
    1 <= x <= z
    x <= y <= z
    эти границы для x y помогут избавится от зацикливания
    которое возникает при x <- [1..] y <- [1..] z <- [1..]
    и поможет избавится от повторов вида (3,4,5) (4,3,5) 

-}
pifagorThree :: [(Int, Int, Int)]
pifagorThree = [(x, y, z) | 
                    z <- [1..],
                    x <- [1..z],
                    y <- [x..z],
                    (x^2 + y^2) == z^2]
{-Проверка-}
test2Solution :: IO ()
test2Solution = do
    print (take 3 pifagorThree)
    print (take 5 pifagorThree)
    print (take 10 pifagorThree)
{-
Результат:
    ghci> test2Solution
    [(3,4,5),(6,8,10),(5,12,13)]
    [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17)]
    [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20),(7,24,25),(15,20,25),(10,24,26),(20,21,29)]
-}



{-Задание 3
Используем pifagorThree для генерации примитивных пифагоров троек
тройка считается примитивной если НОД(x,y) = 1
-}
pifagorPrimitiveThree :: [(Int, Int, Int)]
pifagorPrimitiveThree = [(x, y, z) | 
                            (x, y, z) <- pifagorThree,
                            gcd x y == 1
                        ]
{-Проверка-}
test3Solution :: IO ()
test3Solution = do
    print (take 3 pifagorPrimitiveThree)
    print (take 5 pifagorPrimitiveThree)
    print (take 10 pifagorPrimitiveThree)
{-
Результат:
ghci> test3Solution
[(3,4,5),(5,12,13),(8,15,17)]
[(3,4,5),(5,12,13),(8,15,17),(7,24,25),(20,21,29)]
[(3,4,5),(5,12,13),(8,15,17),(7,24,25),(20,21,29),(12,35,37),(9,40,41),(28,45,53),(11,60,61),(16,63,65)]
-}



{-Задание 4-}
{-
Число является совершенным если равно сумме всех его делителей кроме себя
findAllDivs найдет все делители числа не включая само число 
absoluteNumbers список таких x которые равны sum (findDivs x)
-}
findAllDivs:: Int -> [Int]
findAllDivs x = 1 : helper x 2 (floor (sqrt (fromIntegral x))) []
    where 
        helper num start end acc  
            | (start > end)         = acc
            | (num `mod` start == 0) = 
                let 
                    d = num `div` start 
                    added = if (d == start) 
                        then [start]
                        else [start,d]
                in helper num (start+1) end (added++acc)
            | otherwise = helper num (start+1) end acc
            
absoluteNumbers :: [Int]
absoluteNumbers = [x | x <- [2..], x == sum (findAllDivs x)]
{-Проверка-}
test4Solution :: IO ()
test4Solution = do
    print (take 2 absoluteNumbers)
    print (take 3 absoluteNumbers)
    print (take 4 absoluteNumbers)
{-
Результат:
ghci> test4Solution
[6,28]
[6,28,496]
[6,28,496,8128]
-}

{-Задание 5-}
{-
    Генерируем Канторовые пары 
    (0,0) x + y = 0
    (0,1) x + y = 1
    (1,0) x + y = 1
    (0,2) x + y = 2
    (1,1) x + y = 2
    (2,0) x + y = 2
    getPairs возвращает список всех пар с суммой n
    cantorPairs генерирует бесконечный список всех возможных пар,
    склеивая getPairs для n от 0 до бесконечности
-}
cantorPairs :: (Num a, Enum a, Eq a) => [(a, a)]
cantorPairs = concat [getPairs n | n <- [0..]]
    where getPairs n = [(x, n - x) | x <- [0..n]]

{-Проверка-}
test5Solution :: IO ()
test5Solution = do
    print (take 3 cantorPairs)
    print (take 6 cantorPairs)
    print (take 10 cantorPairs)
{-
Результат:
ghci> test5Solution   
[(0,0),(0,1),(1,0)]
[(0,0),(0,1),(1,0),(0,2),(1,1),(2,0)]
[(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0)]
-}

{-Задание 6-}
{-
    Находит наименьшее расстояние между всеми парами точек из списка.
    Возвращает 0.0 если в списке есть совпадающие точки, 
    иначе возвращает минимум из расстояний между всеми различными точками.
-}

checkDuplicate :: (Eq a) => [a] -> Bool
checkDuplicate []  = False
checkDuplicate [_] = False
checkDuplicate (a:xs)
    | a `elem` xs  = True 
    | otherwise    = checkDuplicate xs 

minDistDots [] = error "Нужно хотя бы 2 точки"
minDistDots [_] = error "Нужно хотя бы 2 точки"
minDistDots xs
    | checkDuplicate xs  = 0.0
    | otherwise = minimum [dotsDist dot1 dot2 | dot1<-xs, dot2 <- xs, dot1/=dot2]
    where 
        dotsDist a b = sqrt ((fst b - fst a) ^ 2 + (snd b - snd a) ^ 2)
{-Проверка-}
test6Solution :: IO ()
test6Solution = do
    print (minDistDots [(1,2), (2,3), (3,4)])
    print (minDistDots [(2,3), (4,5), (6,7)])
    print (minDistDots [(2,3), (2,3), (7,8)])
{-
Результат:
ghci> test6Solution 
1.4142135623730951
2.8284271247461903
0.0
-}
