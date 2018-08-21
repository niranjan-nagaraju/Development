import IO

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x : xs) = x : sieve [p | p <- xs, p `mod` x /= 0]

primes n = (2 : sieve [3, 5 .. n])

rotateList :: Integer -> [Integer] -> Int-> [Integer]
rotateList num list len =
	if len == 1
		then list
		else 
			rotateList rotatednum (list ++ [rotatednum]) (len-1) where
			str = show num
			rotatednum = (read ((tail str) ++ [(head str)])::Integer)

{- Sanitize Inputs that result in leading zeroes -}
sanitizeList :: [Integer] -> Int -> [Integer]
sanitizeList rotatedList len =
	if length(sanitizedList) /= length(rotatedList)
		then [0]
		else sanitizedList
	where
	sanitizedList = [x | x<-rotatedList, ((length (show x)) == len)]

{- NOTE: if digits is 1, then rotatedList is null, returning True, 
 - so no additional steps are needed.
 -}
isCircularprime num primesList =
	(foldl (\x y -> x && (elem y primesList)) True sanitizedList)
	where
		len = length (show num)
		rotatedList = rotateList num [] len
		sanitizedList = sanitizeList rotatedList len

main = do
	let primesList = primes 1000000
	let circularPrimes = [x | x <- primesList, (isCircularprime x primesList)]

	print $ length primesList
	print circularPrimes
	print $ length circularPrimes

{-
 - 78498   
 - [2,3,5,7,11,13,17,31,37,71,73,79,97,113,131,197,199,311,337,373,719,733,919,971,991,1193,1931,3119,3779,7793,7937,9311,9377,11939,19391,19937,37199,39119,71993,91193,93719,93911,99371,193939,199933,319993,331999,391939,393919,919393,933199,939193,939391,993319,999331]
 - 55
 -}
