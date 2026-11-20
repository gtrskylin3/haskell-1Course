import Data.Char (toUpper)
import GHC.IO.Handle (hSetEncoding, hGetContents)

main :: IO()

main = do
    putStrLn "what's ur name?"
    name <- getLine
    let name1 = map toUpper name
    putStrLn ("hi " ++ name1) 

-- ghc seminars/7.hs -o 1

main2 :: IO ()
main2 = do 
    text <- readFile "input.txt"
    writeFile "output.txt" (map toUpper text)
    appendFile "output.txt" (map toUpper text)

main3 :: IO()
main3 = do 
    withFile "input.txt" ReadMode $ \handle -> do 
        hSetEncoding handle utf8 
        text <- hGetContents handle
        putStrLn text 

main4 :: IO()
main4 = do 
    withFile "input.txt" WriteMode $ \source -> do 
        withFile "input.txt" WriteMode $ \dest -> do 
            hSetEncoding source utf8 
            hSetEncoding dest utf8 
            text <- hGetContents source
            hPutStr dest text 
