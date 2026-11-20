
data Point = Point Float Float 
data Shape = Circle Float Float Float 
    | Rectangle Float Float Float Float 
    deriving (Show)

-- data Shape = Circle Point Float 
--     | Rectangle Point Point 
--     deriving (Show)
-- surface :: Shape -> Float 
-- surface (Circle _ r) = pi * r^2
-- surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs(x2 - x1) * abs(y2-y1)

surface :: Shape -> Float 
surface (Circle _ _ r) = pi * r^2
surface (Rectangle x1 y1 x2 y2) = abs(x2 - x1) * abs(y2-y1)

data Person = Person {
    firstName::String,
    lastName::String,
    age:: Int,
    height:: Float,
    phoneNumber::String,
    favIceCream::String
    } deriving (Show, Eq)
guy = Person {
    firstName="Iluxa", 
    lastName="Robocop", 
    age=18, 
    height=179.0, 
    phoneNumber="+79509925651", 
    favIceCream="Niger"
}

data Maybe a = Nothing | Just a 
data Vector a = Vector a a a deriving (Show)

-- ghci> v1 = Vector 'a' 'b' 'c'
-- ghci> :t v1
-- v1 :: Vector Char

vplus :: Num a => Vector a -> Vector a -> Vector a
vplus (Vector a1 b1 c1) (Vector a2 b2 c2) = Vector (a1 +a2) (b1 + b2) (c1 + c2)  


data Day = Monday | Tuesday | Wednesday deriving (Show, Enum, Ord, Eq)

-- можно создавать свои параметризованные типы 
-- аргументы для конструкторов лучше задовать через фигурные скобки 
-- есть автоматические геттеры можно использовать deriving 
-- функции с кастомными типами можно писать используя паттерн матчинг 