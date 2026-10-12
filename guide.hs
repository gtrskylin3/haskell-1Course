
checkLocalhost ip = 
    if ip == "127.0.0.1"  || ip == "0.0.0.0"
        then "its localhost"
        else "no it isn't"

func1 num
    | num == 1 -> "One"
    | num == 2 -> "Two"
    | num == 3 -> "Three"
    | otherwise -> "I don't know"

func2 :: Int -> String
func2 1 = "One"
func2 2 = "Two"
func2 3 = "Three"
func2 _ = "I don't know"
    
func2 num =
    case num of 
        1 -> "One"
        2 -> "Two"
        3 -> "Three"
        _ -> "I don't know"


-- Pattern matching 
factorial 0 = 1 
factorial n = n * factorial (n - 1)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a,b,c) -> a
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

legth' :: (Num b) => [a] -> b
legth' [] = 0 
legth' (_:xs) 1 + length' xs 

bmiTell weight height
    | bmi <= skinny = "дрыщ"
    | bmi <= normal = "пойдет"
    | bmi <= fat = "пухляш"
    | otherwise = "что ты такое?"
    where bmi = weight/height^2
        (skinny, normal, fat) = (18.5, 25,0, 30.0)

cylinger :: (RealFloat a ) => a -> a -> abs
cylinder r h = 
    let sideArea = 2 * pi * r * handle
        topArea = pi * r ^2
    in sedeArea + 2 * topArea


tenTimesFour = (\x y -> x * y) 10 4

(.) f g = \x -> f (g x)
($) :: (a -> b) -> a -> b

main:: IO ()
main = putStrLn (checkLocalhost "173.194.22.100")
main = putStrLn . checkLocalhost $ "173.194.22.100"


countLines mystring = length (lines mystring)

countLines' :: String -> Int
countLines' = length . lines

example = show (x + y)
example' = show $ x + y


