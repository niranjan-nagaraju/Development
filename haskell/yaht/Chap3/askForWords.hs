module AskForWords
	where

import IO
import Data.Char

askForWords :: IO [String]
askForWords = do
	word <- getLine
	if word == ""
		then return []
		else do
			rest <- askForWords
			return (word : rest)

convertUC :: [String] -> [String]
convertUC listOfStr = 
	map ucStr listOfStr
	where
		ucStr = map toUpper

main = do
	putStrLn "Enter Text followed by blank line"
	text <- askForWords
	let uc = convertUC text
	putStrLn "Text Input: "
	mapM_ putStrLn text
	putStrLn "Upper Case Text Input: "
	mapM_ putStrLn uc
