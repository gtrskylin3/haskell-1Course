import Data.List (zip5)
import qualified Data.Map as Map

--Output Fix
{-# LANGUAGE FlexibleInstances #-}
instance {-# OVERLAPPING #-} Show String where
    show x = ['"'] ++ x ++ ['"']

--1 Задание 
data Gender = F | M deriving (Show, Eq) 
listName = ["Оля", "Женя", "Катя", "Женя", "Паша", "Саша", "Саша", "Маша", "Надя", "Юля"] :: [String]
listID = [131, 132, 134, 135, 136, 137, 138, 139, 140, 141] :: [Int]
listGender = [F, F, F, M, M, M, F, F, F, F] :: [Gender]
listAge = [17, 18, 17, 19, 18, 19, 21, 19, 18, 20] :: [Int]
listHasFriends :: [Bool]
listHasFriends = [True, False, True, False, False, True, False, True, True, False] :: [Bool]

assotList = Map.fromList [ (id, (name, gender, age, hasFriend)) 
    | (id, name, gender, age, hasFriend) <- zip5 listID listName listGender listAge listHasFriends
    ]

--2 Задание
mansFromDB = [(id, name) |
        (id, (name, gender, age, hasFriend)) <- Map.toList (
            Map.filter (\ (_,g,_,_) -> g == M) assotList
        )
    ]

{- Проверка -}
test1And2Solution :: IO ()
test1And2Solution = do
    print (Map.size assotList)
    print mansFromDB
{-
Результат:
    10
    [(135,"Женя"),(136,"Паша"),(137,"Саша")]
-}

--3 Задание 
freqCountWithMap arr = adjastFreq arr (Map.fromList [(key, 0) | key <- arr])

adjastFreq [] map = map 
adjastFreq (x:xs) map = adjastFreq  xs (Map.adjust (+1) x map) 

{- Проверка-}
test3Solution :: IO ()
test3Solution = do
    print (freqCountWithMap [1,1,1,1,4,5,6])
    print (freqCountWithMap [1,2,3,4,4,4,4,1])
{-
Результат:
fromList [(1,4),(4,1),(5,1),(6,1)]
fromList [(1,2),(2,1),(3,1),(4,4)]
-}


--4 Задание
instance Num Bool where 
    fromInteger 0 = False 
    fromInteger _ = True 
    x + y = x /= y 
    x * y = x && y
    negate x = x 
    signum x = x 
    abs x = x
{- Проверка -}
test4Solution :: IO ()
test4Solution = do
    print (True + True)            -- xor True + True = False
    print (True + False)           -- xor True + False = True
    print (True * False)           -- and True * False = False
    print (True * True)            -- and True * True = True
    print (fromInteger 1 :: Bool)  -- 1 = True
    print (fromInteger 2 :: Bool)  -- 1 = True
    print (fromInteger 0 :: Bool)  -- 0 = False
{-
Результат:
False
True
False
True
True
False
-}


--5 Задание 
class Logic a where
    neg :: a -> a
    (&&&) :: a -> a -> a
    (|||) :: a -> a -> a
    (==>) :: a -> a -> a
    (!) :: a -> a -> a
    a ! b = neg (a &&& b)
    a &&& b = neg ((neg a) ||| (neg b))
    a ||| b = neg ((neg a) &&& (neg b))
    a ==> b = (neg a) ||| b   


instance Logic String where
    neg s = if null s then "True" else ""
    (&&&) s1 s2 = if not (null s1) && not (null s2) then "True" else "" 
    (|||) s1 s2 = if not (null s1) || not (null s2) then "True" else ""

{- Проверка -}
test5Solution :: IO ()
test5Solution = do
    print (neg "")                
    print (neg "привет")           
    print ("привет" &&& "мир")     
    print ("привет" &&& "")        
    print ("" ||| "мир")           
    print ("" ||| "")          
{-
Результат:
"True"
""
"True"
""
"True"
""
-}

--6 Задание 
type One = Double -> Double
instance Num One where
    f + g = \x -> f x + g x 
    f * g = \x -> f x * g x 
    f - g = \x -> f x - g x 
    negate f = \x -> negate (f x)
    abs f = \x -> abs (f x)
    signum f = \x -> signum (f x)
    fromInteger n = \_ -> fromInteger n 
{-
Результат:
(sin + (^2)) 2.0
4.909297426825682
(sin * (^2)) 2.0
3.637189707302727
-}


