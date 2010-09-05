import Control.Monad
import Data.Char

toUC :: String -> [Char]
toUC = map toUpper 

toLC :: String -> [Char]
toLC = map toLower

main = do
	inStr <- getLine
	putStrLn $ concat ["Input String: ", inStr]
	putStrLn $ concat ["UpperCase: ", toUC inStr, "\nLowerCase: ", toLC inStr] 
