import Data.Array

numbers:: Array Int Int
numbers = array (1,3) [(1, 10), (2, 20), (3, 30)]

numbers2 = listArray (1,3) [10, 20, 30]

-- range (1,20)
-- inRange (1, 20) 10
-- index (1, 5) 1

matrix :: Array (Int, Int) Int 
matrix = array ((0,0), (1,1)) [
    ((0,0), 1), ((0,1), 2), 
    ((1,0), 3), ((1,1), 4)] -- левый вернзний 0 0 правый нижний 1 1 

fib :: Array Int Int 
fib = array (0,10) ([(0,0), (1,1)] ++ [(i,fib ! (i - 1)+ fib ! (i -2)) | i <- [2..10]])

-- nums = array (0,10) ([(10, 10), (9,30)] ++ [(i, nums ! (i+1) + nums ! (i+2)) | i <-[2..10]])

-- listArray ('a', 'c') [1,2,3]


