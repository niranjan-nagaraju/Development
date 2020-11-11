import Control.Exception (assert)
import Sieve

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
