main :: IO ()
main = do 
    text <- readFile "L1.html"
    let countTags = foldl' (\acc x -> if x == '<' then acc+1 else acc) 0 text
    print countTags 

    
