-- comment

{- 
многострочный
комментарий
-}

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

myfoo :: Int -> Int -> Int 
myfoo = undefined
-- f: Z x Z -> Z

foo :: Double -> Double
foo x | x >= 0 = sqrt x 
      | x < 0 = error "Use positive numbers!"

-- data Types

data Bool = False | True deriving 
                            (Read, Show, Eq, Ord, Enum, Bounded)

-- != нету вместо него /= 

{-
Char String
Int Integer Float Double
-}

-- Определения функций в Haskell

-- Pattern Matching

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

f :: Double -> String
f 3.14 = "Pi!"
f _ = "another num"

fact' x | x <= 1 = 1
        | otherwise = x * fact (x -1)


-- nextprime x = if isprime x then x else nextprime (x + 1)

-- Ветвления кода

-- тернарник в си y = x > 9 ? 100 : 200

nosign x = if x >= 0  then x else negate x 
fcase x = case x >= 0 of 
        True -> x
        False -> negate x





        


