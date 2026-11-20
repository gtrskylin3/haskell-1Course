import Prelude hiding ((<>))
-- реализация кольца вычетов по mod 5

data MyAlg = O | I | II | III | IV deriving (Eq, Ord, Read, Show)

-- реализация арифметики

next:: MyAlg -> MyAlg
next O = I 
next I = II 
next II = III
next III = IV
next IV = O

prev :: MyAlg -> MyAlg 
prev O = IV
prev I = O 
prev II = I 
prev III = II 
prev IV = III

--сложение
(<>) :: MyAlg -> MyAlg -> MyAlg 
O <> x = x 
n <> x = next (prev n <> x)

--умножение
(><) :: MyAlg -> MyAlg -> MyAlg
O >< _ = O 
n >< x = (prev n >< x) <> x 

toMyAlg :: Integer -> MyAlg
toMyAlg 0 = O 
toMyAlg 1 = I 
toMyAlg 2 = II 
toMyAlg 3 = III
toMyAlg 4 = IV 
toMyAlg x = toMyAlg (x `mod` 5)

fromMyAlg :: MyAlg -> Integer 
fromMyAlg O = 0
fromMyAlg I = 1 
fromMyAlg II = 2 
fromMyAlg III = 3 
fromMyAlg IV = 4 

instance Num MyAlg where 
    fromInteger = toMyAlg
    x + y = x<>y
    x * y = x><y
    negate 0 = O 
    negate n = prev (negate (prev n))
    signum O = O
    signum _ = I 
    abs = id

