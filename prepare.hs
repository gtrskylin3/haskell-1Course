import Data.List 
-- head [1,2,3,4,5] => 1 
-- tail [1,2,3,4,5] => [2,3,4,5]
-- length [1,2,3,4,5] => 5
-- init [1,2,3,4,5] => [1,2,3,4]
-- null [1,2,3,4,5] => False
-- and [True, False, True] => False
-- or [True, False, True] => True
-- [2 * x | x <- [1,2,3]] => [2,4,6]
-- [2 * x | x <- [1,2,3], x > 1] => [4,6]
-- [(x, y) | x <- [1,2,3], y <- ['a', 'b'] ] => [(1,'a'), (1, 'b'), (2, 'a'), (2, 'b'), (3, 'a'), (e, 'b')]

sum1 :: Num t => [t] -> t
sum1 [] = 0 
sum1 (x:xs) = x + sum1 xs

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) 
    | x `mod` 2 == 0 = x: evens xs 
    | otherwise      = evens xs 

in_range min max x = 
    let in_lower_bound  = min <= x 
        in_upper_bound  = max >= x 
    in 
        in_lower_bound && in_upper_bound

in_range2 min max x = in_lower_bound && in_upper_bound
    where 
        in_lower_bound  = min <= x 
        in_upper_bound  = max >= x

fac n = 
    if n <= 1 then 1
    else n * fac (n - 1)

fac2 n 
    | n <= 1    = 1
    | otherwise = n * fac2 (n - 1)

is_zero 0 = True 
is_zero _ = False 

fac3 n = aux n 1 
    where 
        aux n acc 
            | n <= 1    = acc 
            | otherwise = aux (n-1) (n*acc)

-- List [1,2,3,4,5]
-- [1,2,3,4,5] :: [Integer]
-- [] x:xs -добавляет x в xs
-- 1 : 2 : 3 : 4 : 5 : [] = [1,2,3,4,5]

asc:: Int -> Int -> [Int]
asc n m
    | m < n  = []
    | m == n = [m]
    | m > n  = n : asc (n + 1) m 

addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [x+y | (x,y) <- xs]


elem2 :: (Eq a) => a -> [a] -> Bool 
elem2 _ [] = False 
elem2 el (x:xs) 
    | x == el   = True 
    | otherwise = elem2 el xs 


nub2 :: (Eq a) => [a] -> [a]
nub2 arr = reverse (nub2' arr [])
    where 
        nub2' [] arr = arr
        nub2' (x:xs) arr
            | x `elem` arr = nub2' xs arr
            | otherwise    = nub2' xs [x] ++ arr 

nub3 :: (Eq a) => [a] -> [a]
nub3 [] = []
nub3 (x:xs)
    | x `elem` xs = nub3 xs
    | otherwise   = x : nub3 xs

isAsc :: [Int] -> Bool
isAsc [] = True 
isAsc [_] = True 
isAsc (x:y:xs) 
    | x > y     = False 
    | otherwise =  isAsc (y:xs)


hasPath :: [(Int, Int)] -> Int -> Int -> Bool
-- [(1,2) (2, 3), (3, 2), (4, 3), (4, 5)] 1,3
-- [(1, 2)]
-- [(2, 3)]
    
-- hasPath arr from to = helper (waysFrom from)
--     where
--         waysFrom fromTo = [(x,y) | (x,y) <- arr, x == fromTo]
--         helper [] = False
--         helper (x:xs)
--             | snd x == to = True
--             | hasPath arr (snd x) to = True
--             | otherwise                    = helper xs 


hasPath [] x y = x == y
hasPath xs x y 
    | x == y = True 
    | otherwise = 
        let xs' = [(n, m) | (n, m) <- xs, n /= x]
        in or [hasPath xs' m y | (n,m) <- xs, n == x]


roots a b c = 
    let 
        d = b^2 - (4*a*c)
        sd = sqrt(d)
        r1 = (-b + sd)/2*a
        r2 = (-b - sd)/2*a
    in (r1, r2)

roots2 a b c = (r1, r2)
    where  
        d = b^2 - (4*a*c)
        sd = sqrt(d)
        r1 = (-b + sd)/2*a
        r2 = (-b - sd)/2*a
     