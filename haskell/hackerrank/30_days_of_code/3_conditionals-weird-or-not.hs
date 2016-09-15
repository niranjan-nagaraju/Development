{-
 - https://www.hackerrank.com/challenges/30-conditional-statements
 -} 

import Data.Bits

weirdOrNot :: Int -> Bool
weirdOrNot n 
	| ((.&.) n 0x1 == 1) = True
	| (n >= 2 && n <= 5) = False
	| (n >= 6 && n <= 20) = True
	| otherwise = False


main = do
	n <- readLn :: IO Int
	if weirdOrNot n
		then putStrLn "Weird"
		else putStrLn "Not Weird"

