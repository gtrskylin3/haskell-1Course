--Задание 0 
data ATree a = ALeaf a | ABranch (ATree a) a (ATree a)
   deriving (Show, Read)

tree = ABranch 
    (ABranch 
        (ALeaf 21) 
        2 
        (ABranch 
            (ABranch (ALeaf 62) 43 (ALeaf 66)) 
            22 
            (ALeaf 46)
        )
    ) 
    1 
    (ABranch 
        (ALeaf 34) 3 (ALeaf 35))

tree2 = ABranch 
    (ABranch 
        (ABranch 
            (ALeaf 46)
            22 
            (ABranch (ALeaf 62) 43 (ALeaf 66)) 
        )
        2 
        (ALeaf 21) 
    ) 
    1 
    (ABranch 
        (ALeaf 35) 3 (ALeaf 34))

--Задание 1
treeSum :: (Num a) => ATree a -> a 
treeSum (ALeaf x) = x 
treeSum (ABranch left x right) = x + treeSum left + treeSum right 
{- Проверка -}
test1Solution :: IO ()
test1Solution = do
    print (treeSum tree)
    print (treeSum tree2)

{-
Результат:
ghci> test1Solution
335
335
-}

--Задание 2
evenLeafs:: (Integral a) => ATree a -> [a]
evenLeafs (ALeaf x)
    | even x    = [x]
    | otherwise = []
evenLeafs (ABranch left x right)
    | even x    = x : (evenLeafs left ++ evenLeafs right)
    | otherwise = evenLeafs left ++ evenLeafs right
{- Проверка  -}
test2Solution :: IO ()
test2Solution = do
    print (evenLeafs tree)
    print (evenLeafs tree2)
{-
Результат:
ghci> test2Solution 
[2,22,62,66,46,34]
[2,22,46,62,66,34]
-}

--Задание 3
sisterLeafs:: (Num a) => ATree a -> [(a, a)]
sisterLeafs (ALeaf _) = []
sisterLeafs (ABranch left x right) = sisters left right ++ sisterLeafs left ++ sisterLeafs right
    where 
        sisters (ABranch _ a _) (ABranch _ b _) = [(a,b)]
        sisters (ABranch _ a _) (ALeaf b)       = [(a,b)]
        sisters (ALeaf a) (ABranch _ b _)       = [(a,b)]
        sisters (ALeaf a) (ALeaf b)             = [(a,b)]


{- Проверка -}
test3Solution :: IO ()
test3Solution = do
    print (sisterLeafs tree)
    print (sisterLeafs tree2)

{-
Результат:
ghci> test3Solution 
[(2,3),(21,22),(43,46),(62,66),(34,35)]
[(2,3),(22,21),(46,43),(62,66),(35,34)]
-}


--Задание 4
subTrees :: (Num a) => ATree a -> [ATree a]
subTrees tree = subTrees' tree ++ [tree]
    where 
        subTrees' (ALeaf _) = []
        subTrees' (ABranch left x right) = left : right : subTrees' left ++ subTrees' right

{- Проверка -}
test4Solution :: IO ()
test4Solution = do
    print (subTrees tree)

{-
Результат:
ghci> test4Solution 
[ABranch (ALeaf 21) 2 (ABranch (ABranch (ALeaf 62) 43 (ALeaf 66)) 22 (ALeaf 46)),
ABranch (ALeaf 34) 3 (ALeaf 35),ALeaf 21,
ABranch (ABranch (ALeaf 62) 43 (ALeaf 66)) 22 (ALeaf 46),
ABranch (ALeaf 62) 43 (ALeaf 66),ALeaf 46,ALeaf 62,ALeaf 66,ALeaf 34,ALeaf 35,
ABranch (ABranch (ALeaf 21) 2 (ABranch (ABranch (ALeaf 62) 43 (ALeaf 66)) 22 (ALeaf 46))) 1 (ABranch (ALeaf 34) 3 (ALeaf 35))]
-}


--Задание 5
sameTrees :: Eq a => ATree a -> ATree a -> Bool
sameTrees (ALeaf x) (ALeaf y) = x==y
sameTrees (ABranch left1 x right1) (ABranch left2 y right2)
    | x /= y = False 
    | (sameTrees left1 left2) && (sameTrees right1 right2) = True
    | (sameTrees left1 right2) && (sameTrees right1 left2) = True
    | otherwise = False 
sameTrees _ _ = False 
{- Проверка -}
test5Solution :: IO ()
test5Solution = do
    print (sameTrees tree tree2)
    print (sameTrees tree tree)
    print (sameTrees tree2 tree2)

{-
Результат:
ghci> test5Solution
True
True
True
-}
--Задание 6
instance Eq a => Eq (ATree a) where
    (==) = sameTrees
{- Проверка -}
test6Solution :: IO ()
test6Solution = do
    print (tree == tree2)
    print (tree == tree)
    print (tree2 == tree2)

{-
Результат:
ghci> test6Solution
True
True
True
-}