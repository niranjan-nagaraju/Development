module Sieve (
	sieve,
	first_n_primes,
	nth_prime,
	primes_below,
	primes_between
) where

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


