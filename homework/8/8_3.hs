import Text.Regex.Posix

main :: IO ()
main = do 
    text <- readFile "L1.html"
    let matches = text =~ "<a href=\"(https://[^\"]+)\"" :: [[String]]
    let links = [match !! 1 | match <- matches, length match > 1]
    writeFile "output3.txt" (unlines links)

