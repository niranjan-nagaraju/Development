import IO
import List

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x : xs) = x : sieve [p | p <- xs, p `mod` x /= 0]

primes n = (2 : sieve [3, 5 .. n])

primes100 = primes 100

prime_divisors_and_powers :: Int -> [(Int, Int)]
prime_divisors_and_powers n = 
	sanitized_divisors
	where
		primesList = filter (<=n) primes100
		divisors = prime_divisors_and_powers' n primesList []
		sanitized_divisors = filter (\x -> (snd(x) > 0)) divisors
	
prime_divisors_and_powers' n [] divisors = divisors
prime_divisors_and_powers' n [p] divisors = 
		divisors ++ [(p, nPowers)]
		where
			nPowers = (snd (divisor_power n p 0))
prime_divisors_and_powers' n (px:pxs) divisors = 
	if pxs == []
		then divisors
		else
			prime_divisors_and_powers' new_n pxs (divisors ++ [(px, nPowers)])
			where
				(new_n, nPowers) = divisor_power n px 0

divisor_power n p nPowers = 
	if n `mod` p /= 0
		then (n, nPowers)
		else
			divisor_power (n `div` p) p (nPowers + 1)

-- uniqueApowerB :: [Int] -> [Int] -> [[(Int, Int)]]
uniqueApowerB alist blist = 
	nub (foldl (\x y -> x ++ (curr_exp y)) [] blist)
	where
	adivisors = map (\x -> (prime_divisors_and_powers x)) alist
	add_powers y divisors = map (\x -> (fst(x), (snd(x) * y))) divisors
	curr_exp y = map (\x -> add_powers y x) adivisors

main = do
	print $ length (uniqueApowerB [2..5] [2..5]) {- 15 -}
	print $ length (uniqueApowerB [2..100] [2..100]) {- 9183 -}
