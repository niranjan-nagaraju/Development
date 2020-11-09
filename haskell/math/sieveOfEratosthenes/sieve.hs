import Control.Exception (assert)

{-
 - Build a sieve of eratosthenes recursively
 - Each call, (x:xs) adds x as a prime, and filters out all
 -  i ∈ xs , if x|i
 -}
sieve' :: [Integer] -> [Integer]
sieve' [] = []
sieve' (x : xs) = x : sieve' [i | i <- xs, i `mod` x /= 0]


-- Infinite list of prime numbers, filter lazily using a predicate
sieve :: [Integer]
sieve = (2 : sieve' [3, 5 .. ])


-- Get first 'n' prime numbers using the sieve-of-Eratosthenes method
first_n_primes :: Int -> [Integer]
first_n_primes n = (take n) sieve


-- Get nth prime number
nth_prime :: Int -> Integer
nth_prime n = sieve !! (n-1) -- Get (n-1)th item from the 0-indexed list of primes


-- Get all primes below 'n'
primes_below :: Integer -> [Integer]
primes_below n = takeWhile (<n) sieve


-- Extract all primes between m and n (inclusive)
primes_between :: Integer -> Integer -> [Integer]
primes_between m n = 
	dropWhile (<m) $ takeWhile (<=n) sieve


main = do
	putStr $ assert ( (take 10) sieve == [2,3,5,7,11,13,17,19,23,29] ) ""

	putStr $ assert ( (first_n_primes 10) == [2,3,5,7,11,13,17,19,23,29] ) ""
	putStr $ assert ( (first_n_primes 1) == [2] ) ""

	putStr $ assert ( (nth_prime 1) == 2 ) ""
	putStr $ assert ( (nth_prime 10) == 29 ) ""
	putStr $ assert ( (nth_prime 100) == 541 ) ""

	putStr $ assert ( (primes_below 100) ==
		[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97] ) ""
	putStr $ assert ( (primes_below 10) == [2,3,5,7] ) ""

	putStr $ assert ( (primes_between 7 100) ==
		[7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97] ) ""
	putStr $ assert ( (primes_between 10 53) ==
		[11,13,17,19,23,29,31,37,41,43,47,53] ) ""

	putStrLn "Testcases complete!"
