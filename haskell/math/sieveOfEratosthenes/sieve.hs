import Control.Monad

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x : xs) = x : sieve [p | p <- xs, p `mod` x /= 0]

main = do
	n <- readLn
	print (2 : sieve [3, 5 .. n])
