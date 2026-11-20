main :: IO ()
main = do 
    text <- readFile "L1.html"
    let allLines = lines text
    let every3th = [line | (line,i) <- zip allLines [1..(length allLines)], i `mod` 3 == 0]
    writeFile "output2.txt" (unlines every3th)
    
