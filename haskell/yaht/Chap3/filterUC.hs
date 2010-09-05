import Control.Monad
import Data.Char

filterUC::String->[Char]
filterUC = filter isUpper

main = do
	inStr <- getLine
	putStrLn $ filterUC inStr

