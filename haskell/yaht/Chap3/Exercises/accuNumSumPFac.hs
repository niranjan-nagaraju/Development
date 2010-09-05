module NumbersAcc
	where

import Control.Monad

getNumbers :: IO [Integer]
getNumbers = do
	putStrLn "Enter numbers (0 to stop): "
	num <- (readLn::IO Integer)
	if num == 0
		then return []
		else do
			rest <- getNumbers
			return (num : rest)

printFactorials [] = return ()
printFactorials (x:xs) = do
	putStrLn (show x ++ " Factorial is " ++ show (product [1..x]))
	printFactorials xs

main = do
	numbers <- getNumbers
	putStr ("Input Numbers: ")
	print numbers
	putStrLn ("The Sum is: " ++ show (sum numbers))
	putStrLn ("The product is: " ++ show (product numbers))
	-- mapM_ printFactorial numbers
	printFactorials numbers
