
rotateList :: Integer -> [Integer] -> Int-> [Integer]
rotateList num list len =
	if len == 1
		then list
		else 
			rotateList rotatednum (list ++ [rotatednum]) (len-1) where
			str = show num
			rotatednum = (read ((tail str) ++ [(head str)])::Integer)

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
	let primesList = [2,3,5,7,11,13,17,31,37,71,73,79,97,101,103,107,113,131,197,199,307,311,337,373,701,709,719,733,907,919,971,991,1013,1031,1097,1103,1193,1301,1931,3001,3011,3037,3119,3779,7001,7019,7109,7793,7937,9007,9091,9311,9377,10007,10099,10103,10193,10301,10909,11701,11939,13001,19391,19937,30011,30119,31019,37199,39119,70001,70003,70009,70019,70793,70937,71993,77093,90007,90019,90071,91009,91193,93719,93911,97001,99371,100003,100103,100193,100931,101939,103001,109391,109937,110939,119033,193939,199933,300007,300073,300119,300779,307079,310019,319993,331999,390119,391103,391939,393919,700001,700109,700303,700937,900007,900019,900091,900701,901009,901193,909371,913907,919393,930011,930077,930707,933199,939011,939193,939391,990001,990719,993319,997103,999331]
	let circularPrimes = [x | x <- primesList, (isCircularprime x primesList)]

	print primesList
	putStrLn ""
	print circularPrimes
	print $ length circularPrimes


