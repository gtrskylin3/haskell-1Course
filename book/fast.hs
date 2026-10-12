-- В Haskell функция — это прежде всего
--  описание зависимости результата 
-- от входных данных.
main :: IO ()

doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = 
    if x > 100
        then x
        else x * 2 -- required

square :: Num a => a -> a
square x = x * x

average :: Fractional a => a -> a -> a
average x y = (x + y) / 2

isBig :: (Ord a, Num a) => a -> Bool
isBig x = x > 100 


isAdult :: (Ord a, Num a) => a -> Bool
isAdult x = x >= 18

-- Num говорит: с a можно выполнять числовые операции.
-- Ord говорит: значения a имеют порядок, их можно сравнивать через <, >, <=, >=.

isSame :: (Eq a) => a -> a -> Bool
isSame x y = x == y

-- Сопоставления с образцами

lucky :: Int -> String
lucky 1 = "one"
lucky 2 = "two"
lucky x = "Hello world"

-- можно испольлзовать не только с числами
myHead :: [a] -> a
myHead []  = error "Empty List!"
myHead (x:_) = x  -- (x:_) есть первый элемент

-- Input
-- getLine :: IO String

-- OutPut 
-- main :: IO ()
-- main = putStrLn "Hello"

main = do
    name <- getLine
    putStrLn ("Привет " ++ name)
-- do — последовательность действий

main2 = do 
    putStrLn "Привет как тебя зовут?"
    name <- getLine
    putStrLn ("Привет, " ++ name ++ ", от  HASKELL!")

helloUser:: IO ()
helloUser = do
    putStrLn "Как тебя зовут?"
    name <- getLine
    putStrLn "Сколько тебе лет?"
    age <- getLine
    putStrLn ("Привет, " ++ name ++ "! " ++ "Тебе " ++ age ++ " лет.")

    