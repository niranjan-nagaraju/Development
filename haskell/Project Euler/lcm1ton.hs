import IO

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x : xs) = x : sieve [p | p <- xs, p `mod` x /= 0]

{- Lists all primes <= n -}
primes :: Integer -> [Integer]
primes n = (2 : sieve [3, 5 .. n])

{- Returns maximum exponent m in the prime factorization of n with the prime a as base,
 - i.e. a^m * d = n, where a is prime, m is max
 -	{- for e.g. for 2, 24 it returns 2.. because 12 = 2^2 * 3 -}
 -}
nExponent :: Integer -> Integer -> Integer
nExponent a n =
	if n `mod` a /= 0
		then 0
		else (1 + nExponent a (n `div` a))

{- Returns Maximum p's exponent of a given set of numbers
 - for e.g. for [2,4,6,8,10] and p=2 the exponents list'd be [1,2,1,3,1]
 - the exponents of prime factorization where 2 is the prime number.
 - And max'd be 3 for (8 = 2^3)
 -}
maxExponent :: [Integer] -> Integer -> Integer
maxExponent numList p = maximum (map (nExponent p) numList)

{- 
 - Read n
 - Get list of Primes <= n
 - Get the maximum exponent for each prime p in the prime factorizations of [1 .. n] {- well [2 .. n] -}
 - foreach (m, n) m <- primes, n <- exponents, calculate m^n and multiply the results
 -}
main = do
	n <- readLn 
	
	let primesList = primes n
	let numList = [2 .. n]
	let primeFactorsExp = map (maxExponent numList) primesList
	let lcm = product $ zipWith (^) primesList primeFactorsExp
	
	print lcm
