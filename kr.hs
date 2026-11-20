myLast:: [a] -> a 
myLast [x] = x -- last element
myLast (x:xs) = myLast (xs)

myButLast:: [a] -> a 
myButLast [x, y] = x
myButLast (x: xs) = myButLast xs 

elementAt:: [a] -> Int -> a
elementAt arr idx = arr !! (idx-1)

elementAt2:: [a] -> Int -> a
elementAt2 [] _    = error "индекс большой"
elementAt2 (x:xs) idx 
    | idx < 1     = error "индекс должен быть больше 1"
    | idx == 1     = x
    | otherwise    = elementAt2 xs (idx-1)   


myLength:: [a] -> Int 
myLength [] = 0
myLength (x:xs) = 1 + myLength xs 


myReverse :: [a] -> [a]
myReverse arr = helper arr ((length arr) - 1)  
    where 
        helper arr size 
            | size == -1 = [] 
            | otherwise = arr !! size : helper arr (size - 1) 

myReverse2 :: [a] -> [a]
myReverse2 arr = helper arr []
helper [] acc     = acc 
helper (x:xs) acc = helper xs (x : acc) 


isPalindrome:: (Eq a) => [a] -> Bool 
isPalindrome a =  ((reverse a) == a)


data NestedList a = Elem a | List [NestedList a]

tst :: NestedList Int
tst = List [
        Elem 1,
        List [Elem 2, Elem 3],
        List [List [Elem 4]]
    ]

flatten:: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List ([])) = []  
flatten (List (x:xs)) = flatten x ++ flatten (List (xs))  

flatten2:: NestedList a -> [a]
flatten2 (Elem a) = [a]
flatten2 (List (xs)) = concat (map flatten2 xs)  

compress:: Eq a =>  [a] -> [a]
compress arr = helper2 arr []
    where 
        helper2 [] acc = acc 
        helper2 (x:xs) acc 
            | x `elem` acc = helper2 xs acc 
            | otherwise    = helper2 xs (acc++[x])

pack:: (Eq a) => [a] -> [[a]]
pack [] = []
pack arr = reverse (helper arr [])
    where 
        helper [] res = res  
        helper (x:xs) res =  helper [t | t <- xs, t /= x] ([t | t <- arr, t == x]:res)


-- compress1 :: Eq a => [a] -> [[a]]
-- compress1 list = foldr go [] list
--     where go x [] = [[x]]
--           go x acc
--             | x == head (head acc) = (x:head acc):tail acc
--             |otherwise = [x] : acc

encode arr = [(length x, (head x)) | x <- pack arr]

dupli [] = []
dupli (x:xs) = x : x : dupli xs

repli arr count = repli' arr count count 
    where 
        repli' [] _ _ = []
        repli' (x:xs) cur count
            | cur == 0 = repli' xs count count  
            | otherwise = x : repli' (x:xs) (cur-1) count

repli2 arr count = concatMap (replicate count) arr 

dropEvery arr n = [x | (idx, x) <- zip [1..] arr, (idx `mod` n) /= 0]

split arr idx = (beforeIdx arr idx, afterIdx 0 arr idx)
    where 
        beforeIdx _ 0 = []
        beforeIdx [] _ = []
        beforeIdx (x:xs) idx = x : beforeIdx (xs) (idx-1)

        afterIdx _ [] _ = []
        afterIdx cur (x:xs) idx 
            | cur >= idx = x : afterIdx (cur+1) xs idx
            | otherwise = afterIdx (cur+1) xs idx

split2 [] _ = ([], [])
split2 list 0 = ([], list)
split2 (x:xs) n = (x : left, right)
    where 
        (left, right) = split2 xs (n - 1)

removeAt k arr = removeAt' k 0 arr
    where 
        removeAt' k idx (x:xs)
            | idx == k = () 


catdog [] ys = ys 
catdog (x:xs) ys = x : catdog xs ys

oddsOnly :: Integral a => [a] -> [a]

oddsOnly [] = []
oddsOnly (x:xs) 
    | odd x = x : oddsOnly xs
    | otherwise = oddsOnly xs 


last1 :: [a] -> a
last1 (x:[]) = x 
last1 (_:xs) = last1 xs 

init1 :: [a] -> [a]
init1 [x] = []
init1 (x:xs) = x : init1 xs

isPalindrome1 :: Eq a => [a] -> Bool
isPalindrome1 [] = True
isPalindrome1 [_] = True
isPalindrome1 (x:xs)
    | x /= last xs = False
    | otherwise    = isPalindrome1 (init xs)


myZip :: [a] -> [b] -> [(a,b)]
myZip []  _ = []
myZip _  [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myZip3 (a:as) (b:bs) (c:cs) = (a,b,c) : myZip3 as bs cs 
myZip3 _ _ _ = []

myUnzip :: [(a,b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((x,y):xys) = (x:xs, y:ys)
    where 
        (xs, ys) = myUnzip xys
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []                                      -- Все списки закончились
sum3 (a:as) (b:bs) (c:cs) = (a + b + c) : sum3 as bs cs  -- Все три списка еще идут
sum3 (a:as) (b:bs) []     = (a + b)     : sum3 as bs []  -- Закончился третий
sum3 (a:as) []     (c:cs) = (a + c)     : sum3 as [] cs  -- Закончился второй
sum3 []     (b:bs) (c:cs) = (b + c)     : sum3 [] bs cs  -- Закончился первый
sum3 (a:as) []     []     = a           : sum3 as [] []  -- Остался только первый
sum3 []     (b:bs) []     = b           : sum3 [] bs []  -- Остался только второй
sum3 []     []     (c:cs) = c           : sum3 [] [] cs  -- Остался только третий

-- groupElems :: Eq a => [a] -> [[a]]
-- groupElems arr = helper arr [] []
--     where
--         helper [] cur res = cur : res 
--         helper (x:y:xs) cur res 
--             | x == y =  helper (y:xs) (x:cur) res 
--             | otherwise = helper (y:xs) [] (cur : res)

