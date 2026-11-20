import Text.Regex.Posix

main :: IO ()
main = do 
    text <- readFile "L1.html"
    let matches  = text =~ "<[hH][1-6]>([^<]+)</[hH][1-6]>" :: [[String]]
    let result = [match !! 1 | match <- matches, length match > 1]
    writeFile "output4.txt" (unlines result) 


