-- data Notes = Notes [String] deriving (Eq,Show, Read)
newtype Notes = Notes [String] deriving (Eq,Show, Read, Semigroup, Monoid)
-- data Notes = Notes [String] deriving (Eq,Show, Read)
first = Notes ["read", "write"]
second = Notes ["123"]

joinNotes (Notes xs) (Notes ys) = Notes (xs ++ ys)

-- instance Semigroup Notes where 
--     (<>) = joinNotes

-- instance Monoid Notes where
--     mempty = Notes []

data Box a = Box a deriving (Show, Read, Eq)

firstB = Box 1
secondB = Box 2
addOne (Box x) = Box (x+1)

mapBox f (Box x) = Box (f x)

instance Functor Box where
    fmap = mapBox

addBoxes (Box s) (Box y) = Box (s+y)

instance Applicative Box where 
    Box f <*> Box x  = Box (f x)
    pure = Box 