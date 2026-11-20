import Data.List (isPrefixOf)
--2 часть 
data Person = Person {
    name::String,
    age::Int,
    weight::Double
}  deriving (Eq, Show, Read)

p1::Person
p1 = Person {
    name = "Winnie the Pooh",
    age = 7,
    weight = 20.5
}

serializePerson :: Person -> [String]
serializePerson p = [
    "name" ++ " = " ++ (show (name p)), 
    "age" ++ " = " ++ (show (age p)),
    "weight" ++ " = " ++ (show (weight p))
    ]

parsePerson :: String -> Person
parsePerson inputData =
    let 
        strings = lines inputData
        getValue key = if not (null res)
                        then drop (length (key ++ " = ")) (head res) 
                        else error "Can't find value"   
            where 
                res = (filter (isPrefixOf (key ++ " = ")) strings)

        rawName = getValue "name"
        rawAge = getValue "age"
        rawWeight = getValue "weight"
    in Person
        { 
          name   = read rawName,
          age    = read rawAge,
          weight = read rawWeight
        }


main = do 
    -- writeFile "output2.txt" (unlines (serializePerson p1))
    text <- readFile "output2.txt"
    let p2 = parsePerson text 
    print p2 