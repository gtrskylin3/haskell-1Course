{-Задание 1-}
{-
    Находим наименьшее натуральное число 
    отсутствующее в последовательности.
    
    Начиная с 0 проверяем входит ли число в последовательность
    Увеличиваем его пока не найдем такое,
    которого нет в последовательности.
-}
f1 :: [Int] -> Int
f1 arr = check 0 arr 
    where 
        check el arr 
            | (el `elem` arr) == False = el 
            | otherwise                = check (el+1) arr

{-Проверка-}
test1Solution :: IO ()
test1Solution = do
    print (f1 [0..1000])
    print (f1 [23,7,44,5,1,5,2,8,11,3,7,6,0,9])
    print (f1 [])
{-
Результат:
ghci> test1Solution
1001
4
0
-}


{-Задание 2-}
{-
Получаем отсортированный по неубыванию массив list3
состоящий из всех элементов списков list1 и list2 

На каждом этапе склеивания списковсравниваем их первые элементы 
минимальный из них вставляем в начало нашего нового списка,
который строится на ходу.
Если мы достигли конца одного из списков,
то полученый на ходу склеиваем с другим
-}
sortTwoLists :: Ord a => [a] -> [a] -> [a]
sortTwoLists [] xss = xss
sortTwoLists xs [] = xs
sortTwoLists (a:xs) (b:xss) 
    | (a <= b)  = a: sortTwoLists xs (b:xss) 
    | otherwise =  b: sortTwoLists (a:xs) xss 
{-Проверка-}
test2Solution :: IO ()
test2Solution = do
    print (sortTwoLists [1,4,4,6,73,543,624] [2,2,4,6,8,8,32,44,56,78,87,89,91,95])
{-
Результат:
ghci> test2Solution
[1,2,2,4,4,4,6,6,8,8,32,44,56,73,78,87,89,91,95,543,624]
-}


{-Задание 3-}
{-
    Подсчитывает сколько раз повторяется элемент 
    в списке.
    Проходимся по списку через свёртку и 
    проверяем равны ли текущий элемент и заданый
    накапливая кол-во равных (прибавляя 1 к нулю)
-}
countDuplicateElements:: (Num a, Eq a) => a -> [a] -> a 
countDuplicateElements elem lst = foldl (\counter x -> if elem==x then counter+1 else counter) 0 lst
{-Проверка-}
test3Solution :: IO ()
test3Solution = do
    print (countDuplicateElements 2 [1,2,3,4,2,2,2])
    print (countDuplicateElements 2 [1,2,3,4])
    print (countDuplicateElements 2 [1,3,4])
    print (countDuplicateElements 2 [])
{-
Результат:
ghci> test3Solution   
4
1
0
0
-}


{-Задание 4-}
{-
    Находим мажорирующий элемент списка длинны n
    это такой элемент, который встречается более чем n/2 раз.
    Проверям количество повторов через функцию реализованую в 3 задании.
    
    Рекурсивно отщепляем по элементу пока не найдем тот
    что удовлетворяет нужному условию (countDuplicateElements a (a:xs) > majorCount)
    иначе выводим исключение что нужного элемента нет
-}
findMajorElement :: [Int] -> Int
findMajorElement [] = error "Пустой список"
findMajorElement lst = helper lst 
    where 
        majorCount = length lst `div` 2 
        helper [] = error "Мажорирующего элемента в списке нет."
        helper (a:xs)  
            | (countDuplicateElements a (a:xs) > majorCount) = a
            | otherwise                                      = helper xs
{-Проверка-}
test4Solution :: IO ()
test4Solution = do
    print (findMajorElement [3,3,4,2,4,4,2,4,4])
    print (findMajorElement [3,3,4,2,4,4,2,4])
{-
Результат: 
ghci> test4Solution   
4
*** Exception: Мажорирующего элемента в списке нет.
-}