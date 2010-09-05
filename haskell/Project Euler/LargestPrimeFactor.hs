sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x : xs) = x : sieve [p | p <- xs, p `mod` x /= 0]

{- Lists all primes <= n -}
primes :: Integer -> [Integer]
primes n = (2 : sieve [3, 5 .. n])

maxPrimeFactor primesList n = 
	head $ dropWhile (\x -> (n `mod` x) /= 0) primesList

main = do
	-- print $ maximum (primes 600851475143)
	-- let n = 600851475143
	n <- readLn
	let srt = round $ sqrt (fromInteger n)
	print (maxPrimeFactor (reverse (primes srt)) n)
