-- Импорты для дальшнейшей работы с Rational
import Data.Ratio (denominator, numerator, (%))

{-ЗАДАНИЕ 1-}
-- Слишком медленно вычисляет для больших n из за повторыных вычислений рекурсии
-- пороговое значение для моей машины: n = 40
fibonacciSlow :: Int -> Integer
fibonacciSlow 0 = 0
fibonacciSlow 1 = 1
fibonacciSlow n = fibonacciSlow (n - 1) + fibonacciSlow (n - 2)

-- Динамически вычисляет значение n-го числа Фибоначчи за O(n) рекурсивных вызовов
-- функция helper помнит текущий, следующий элемент, номер элемента который нужно найти  
-- шаг рекурсии  делает текущий = следующий, а следующий = текущий + следующий
-- Пример работы helper для n = 5 
-- 1 шаг 0 1 5 
-- 2 шаг 1 1 4 
-- 3 шаг 1 2 3 
-- 4 шаг 2 3 2 
-- 5 шаг 3 5 1 
-- 6 шаг 5 8 0 выводим 5 и завершаем поиск т.к достигли нуля 
-- на примере видно как current образует последовательность Фибоначчи
fibonacci:: Int -> Integer
fibonacci n = helper 0 1 n
    where helper current next count
            | count <= 0   = current 
            | otherwise = helper next (current+next) (count-1)
-- Тестирование 
testFirstSolution :: IO ()
testFirstSolution = do
    putStrLn ((show (fibonacci 10)) ++ "\n\n") -- output 55
    putStrLn ((show (fibonacci 100)) ++ "\n\n") -- output 354224848179261915075
    putStrLn ((show (fibonacci 1000)) ++ "\n\n") -- output 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875


{-ЗАДАНИЕ 2-}
harmsum :: Integer -> Double
harmsum n = sum [1/ (fromIntegral k) | k <- [1..n]]
-- Тестирование 
testSecondSolution :: IO ()
testSecondSolution = do
    putStrLn ((show (harmsum 10)) ++ "\n\n") -- output 2.9289682539682538
    putStrLn ((show (harmsum 100)) ++ "\n\n") -- output 5.187377517639621
    putStrLn ((show (harmsum 1000)) ++ "\n\n") -- output 7.485470860550343

{-ЗАДАНИЕ 3-}
-- Через метод дихотомии solver находит решения для уравнений вида f(x)=0
-- на отрезке [a; b] с заданной точностью eps
-- Как работает дихотомия 
-- берем d1 = (a + b) / 2 и смотрим значение функции для него f(d1)  
-- проверяем выполняются ли условия f(x) = 0 или abs f(d1) < eps тогда считаем что корень найден с заданной точностью
-- Если f(d1) < 0, то делим пополам отрезок [d1; b] и рекурсивно продолжаем  поиск solver func d1 b eps
-- Ecли f(d1) > 0, то делим пополам отрезок [a; d1] и рекурсивно продолжаем поиск solver func a d1 eps

solver :: (Double -> Double) -> Double -> Double -> Double -> Double
solver f a b eps 
    | (value == 0 || abs value < eps) = d1
    | value < 0                       = solver f d1 b eps
    | otherwise                       = solver f a d1 eps
        where 
            d1 = (a + b) / 2
            value = f d1

{- Тестирование для f(x) = 2**x - x^2 -}
func :: Double -> Double
func x = 2**x - x^2
test3Solution :: IO ()
test3Solution = do 
    print ("Test [-2; 0]")
    putStrLn ("eps = 0.1 " ++ show (solver func (-2) 0 0.1))
    putStrLn ("eps = 0.01 " ++ show (solver func (-2) 0 0.01))
    putStrLn ("eps = 0.001 " ++ show (solver func (-2) 0 0.001))
    print ("Test [3; 5]")  
    putStrLn ("eps = 0.1 " ++ show (solver func 3 5 0.1))
    putStrLn ("eps = 0.01 " ++ show (solver func 3 5 0.01))
    putStrLn ("eps = 0.001 " ++ show (solver func 3 5 0.001))
{-
    Из условия что на отрезке [a, b] функция монотонна и f(a), f(b) разных знаков 
    подойдут отрезки:
    1. [-2, 0] т.к 
    f(-2) = 2^(-2) - (-2)^2 = 0.25 - 4 = -3.75 (< 0)
    f(0) = 2^0 - 0^2 = 1 (> 0)
    
    2. [3, 5] т.к
    f(3) = 2^3 - 3^2 = 8 - 9 = -1 (< 0)
    f(5) = 2^5 - 5^2 = 32 - 25 = 7 (> 0)

    Output:
    ghci> test3Solution
    "Test [-2; 0]"
    eps = 0.1 -0.75
    eps = 0.01 -0.765625
    eps = 0.001 -0.7666015625
    "Test [3; 5]"
    eps = 0.1 4.0
    eps = 0.01 4.0
    eps = 0.001 4.0
-}

{-ЗАДАНИЕ 4-}
arctan:: Double -> Int -> Double
arctan x n = sum [((-1)^(k-1) * x^(2*k-1)) / fromIntegral (2*k-1) | k <- [1..n]]
-- Формула работает только для x из [-1;1] !
{-Тестирование-}
test4Solution:: IO ()
test4Solution = do
    putStrLn ("n = 50, x = 0.4, arctg 0.4 = " ++ (show (arctan 0.4 50)))
    putStrLn ("n = 100, x = 0.4, arctg 0.4 = " ++ (show (arctan 0.4 100)))
    putStrLn ("n = 150, x = 0.4, arctg 0.4 = " ++ (show (arctan 0.4 150)))
    putStrLn ("n = 200, x = 0.4, arctg 0.4 = " ++ (show (arctan 0.4 200)))
{-
Output:
n = 50, x = 0.4, arctg 0.4 = 0.380506377112365
n = 100, x = 0.4, arctg 0.4 = 0.380506377112365
n = 150, x = 0.4, arctg 0.4 = 0.380506377112365
n = 200, x = 0.4, arctg 0.4 = 0.380506377112365
-}

{-ЗАДАНИЕ 5-}
-- Вычисления arctan с заданной точностью eps
generateArctan :: Double -> Double -> Integer -> Double
generateArctan x eps k 
    | abs value < eps   = value 
    | otherwise         = value + generateArctan x eps (k+1) 
    where 
        value = (-1)^(k-1) * x^(2*k-1) / fromIntegral (2*k-1)

arctanAuto :: Double -> Double -> Double
arctanAuto x eps  = generateArctan x eps 1
       
{-Тестирование-}
test5Solution:: IO () 
test5Solution = do 
    let x = 0.4
    putStrLn (show (arctanAuto x 0.1)) 
    putStrLn (show (arctanAuto x 0.01)) 
    putStrLn (show (arctanAuto x 0.001)) 
    putStrLn (show (arctanAuto x 0.0001)) 
{-
Output:
ghci> test5Solution   
0.3786666666666667
0.3807146666666667
0.3804806095238095
0.38050973663492066
-}

{-ЗАДАНИЕ 6-}
-- Rational это множество всех дробей (Q)
-- m % n представление дроби m/n в haskell
arctanAutoRational :: Rational -> Rational -> Rational
arctanAutoRational x eps  = generateArctan x eps 1
    where 
        generateArctan x eps k 
            | abs value < eps   = value 
            | otherwise         = value + generateArctan x eps (k+1) 
            where 
                value = (-1)^(k-1) * x^(2*k-1) / fromIntegral (2*k-1)

{-Тестирование-}
test6Solution:: IO () 
test6Solution = do 
    let x = 0.4
    putStrLn (show (arctanAutoRational x 0.1)) 
    putStrLn (show (arctanAutoRational x 0.01)) 
    putStrLn (show (arctanAutoRational x 0.001)) 
    putStrLn (show (arctanAutoRational x 0.0001)) 
{-
Output:
ghci> test6Solution   
142 % 375
17846 % 46875
624226 % 1640625
46820534 % 123046875
-}

{- 
Разница Rational с Double 
1. Rational вычисляет значения без потери точности пока не упрется в оперативную память в отличии от Double который ограничен 64 битами.
2. Rational работает медленнее на больших числах так как приводит к общему знаменателю на каждом шаге
3. Формат вывода: Double - десятичная дробь. Rational - обыкновеная дробь записывается как m % n в haskell  
-}

{-ЗАДАНИЕ 7-}

colculationPiNumber :: Double -> Double
-- arctan(1) = pi/4 => p = arctan(1)*4
colculationPiNumber eps =  4 * arctanAuto 1 eps;
{-Тестирование-}
test7Solution:: IO ()
test7Solution = do
    putStrLn (show (colculationPiNumber 0.1))
    putStrLn (show (colculationPiNumber 0.01))
    putStrLn (show (colculationPiNumber 0.001))
    putStrLn (show (colculationPiNumber 0.0001))
{-
Output:
    ghci> test7Solution
    2.976046176046176
    3.16119861298705
    3.143588659585787
    3.141792613595793
-}

{-ЗАДАНИЕ 8-}
{-
printDecimalSigns - Выводит n десятичных знаков после запятой у Rational числа
numerator - числитель Rational числа 
denominator - знаменатель Rational числа 
longDivision - выводит n знаков после запятой реализуя деление в столбик
на вход подаем (m `mod` n) * 10 то есть отбрасываем целую часть числа и умножаем на 10 для продолжения деления в столбик.
пока counter не дойдет до нуля выводим очередную цифру частного от деления остатка на делитель и накапливаем результат в строку.
-}
printDecimalSigns:: Rational -> Int -> String
printDecimalSigns m n = longDivision ((chisl `mod` znam) * 10) znam n ""
    where 
        chisl = (numerator m)
        znam = (denominator m)
        longDivision num divisor counter result
            | (counter == 0) = result 
            | otherwise      = longDivision ((num `mod` divisor) *10) divisor (counter-1) (result++(show (num `div` divisor)))
{-Тестирование-}
test8Solution :: IO ()
test8Solution = do
    --
    -- 2/3 = 0.666666666666666666666...
    putStrLn (printDecimalSigns (2 % 3) 10)
    -- 4/3 = 1.33333333333333333333333...
    putStrLn (printDecimalSigns (4 % 3) 20)
    -- 10/9 = 1.111111111111111111111111...
    putStrLn (printDecimalSigns (10 % 9) 40)
{-
Output:
ghci> test8Solution   
6666666666
33333333333333333333
1111111111111111111111111111111111111111
-}