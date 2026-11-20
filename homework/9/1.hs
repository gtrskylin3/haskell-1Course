--1 часть
data Person = Person {
    name::String,
    age::Int,
    weight::Double
}  deriving (Eq, Show, Read)

p1::Person
p1 = Person {
    name = "Daniil",
    age = 18,
    weight = 65.5
}

main :: IO ()
main = do 
    writeFile "output1.txt" (show p1)
    text <- readFile "output1.txt" 
    let p2 = read text :: Person
    print p2