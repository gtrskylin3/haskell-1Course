{-# LANGUAGE FlexibleInstances #-}
newtype Si a = Si a deriving (Read, Show)

joinSi (Si x) (Si y) = Si (x + y) 

--1.1
instance Semigroup (Si Int) where
    (<>) = joinSi

instance Monoid (Si Int) where
    mempty = Si 0
--1.2
mapSi f (Si x) = Si (f x)
instance Functor Si where 
    fmap = mapSi 
    
instance Applicative Si where 
    (Si a) <*> (Si b) = Si (a b) 
    pure = Si 


--2
data Lst a = Nil | Cons a (Lst a) deriving (Read, Show)

lst1 = Cons 10 (Cons 20 (Cons 30 Nil))
lst2 = Cons 40 (Cons 50 (Cons 60 Nil))

--2.0
toList :: Lst a -> [a]
toList Nil  = []
toList (Cons x xs) = x : toList xs

fromList :: [a] -> Lst a
fromList [] = Nil 
fromList (x:xs) = Cons x (fromList xs)

joinLst :: Lst a -> Lst a -> Lst a
joinLst a b = fromList ((toList a) ++ (toList b))  

--2.1
instance Semigroup (Lst a) where 
    (<>) = joinLst
instance Monoid (Lst a) where 
    mempty = Nil 

--2.2
mapLst :: (a1 -> a2) -> Lst a1 -> Lst a2
mapLst f xs = fromList (map f (toList xs)) 

instance Functor Lst where 
    fmap = mapLst

instance Applicative Lst where 
    xs <*> ys = fromList ((toList xs) <*> (toList ys))
    pure x = Cons x Nil


--3 
data Struc a = Mk [a] (a,a) deriving (Read, Show)

--3.1
mapStruc f (Mk xs (a,b)) = Mk (map f xs) (f a, f b)    
instance Functor Struc where 
    fmap = mapStruc


-- TODO
-- 1.3
-- 2.3
-- 3.2