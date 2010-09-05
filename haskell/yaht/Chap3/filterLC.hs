import Control.Monad
import Data.Char

filterLC::String->[Char]
filterLC = filter isLower

main = do
	inStr <- getLine
	putStrLn $ filterLC inStr

