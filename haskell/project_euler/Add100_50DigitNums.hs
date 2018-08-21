import IO
import Control.Monad

{-- Project Euler Problem 13 --}

readNIntegers :: Integer -> IO [Integer]
readNIntegers n = do
	if n == 0
		then return []
		else do
			s <- (readLn:: IO Integer)
			rest <- readNIntegers (n-1)
			return (s:rest)

main = do
	numbers <- readNIntegers 100
	let sum = foldr (+) 0 numbers
	let dgts = read (take 10 (show sum))::Integer
	print dgts
	
