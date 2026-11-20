import GHC.IO.Encoding
import System.IO 
import Data.Char
import Text.Regex.Posix 

main :: IO ()
main = do 
    text <- readFile "L1.html"
    let matches = text =~ "<[hH][1-6]>([^<]+)</[hH][1-6]>" :: [[String]]
    let result = [match !! 1 | match <- matches, length match > 1]
    cp <- mkTextEncoding "cp866"
    writehandle <- openFile "output5.txt" WriteMode
    hSetEncoding writehandle cp
    hPutStr writehandle (unlines result) 
    hClose writehandle

