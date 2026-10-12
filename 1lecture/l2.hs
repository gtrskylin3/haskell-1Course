roots a b c = 
    let d = b^2 - 4 * a * c
        sd = sqrt(d)
        x1 = (-b + sd)/(2*a)
        x2 = (-b - sd)/(2*a)
    in (x1, x2)

-- Currying 
f' :: Num a => (a, a) -> a
f' (x, y) = x + y
f'' :: Num a => a -> a -> a
f'' x y = x + y 

square x = x * x
-- лямбда-версия
mi1 = (\x -> x * x) 5
mi2 = \x -> (x - 2)
trio = \x y z -> x*y + z
--Применение
double x = x + x
z = map (\x -> x + x) [1,2,3,4]
z2 = map double [1,2,3,4]
z3 = map (\x -> if x >= 0 
            then x
            else negate x) 
        [1,-2,3,-4]