import Control.Exception (assert)
import Sieve

{-
 - Twin primes are pairs of primes that are 2 numbers apart.
 - e.g, {3,5}, {5,7}, {11,13} etc
 -}


twin_primes' :: [Integer] -> [(Integer, Integer)]
twin_primes' (x:y:rest)
	| y-x == 2 = (x,y) : twin_primes' (y:rest)
	| otherwise = twin_primes' (y:rest)


twin_primes :: [(Integer, Integer)]
twin_primes = twin_primes' sieve


main :: IO()
main = do
	-- first 10 twin-primes
	putStr $ assert ( (take 10 $ twin_primes) ==
		[(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)] ) ""

	-- 11th twin-primes pair	
	putStr $ assert ( (twin_primes !! 10) == (137, 139) ) ""

	putStrLn "Testcases complete!"
