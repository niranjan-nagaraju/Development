{-
 - https://www.hackerrank.com/contests/projecteuler/challenges/euler001
 -
 - If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
 - The sum of these multiples is 23.
 -
 - Find the sum of all the multiples of 3 or 5 below N.
 -
 - SOLUTION APPROACH:
 -    if N is 10, multiples of 3, there are 3 multiples of 10/3 under 3 -> 3,6,9 
 -    sum of multiples of 3 => 3(1+2+3) == 3 * sigma(10/3)
 -       for any N, sum of multiples of 3 under N => 3 * sigma(N/3), {sigma x == sum [1..x]}
 -
 -    Similarly, sum of multiples of 5 under N => 5 * sigma(N/5)
 -       [or sigma(N-1)/5 not counting N itself]
 -
 -    However, counting just sum(multiples of 3 + multiples of 5), we'd be counting multiples of 15 twice
 -      once for 3, and again for 5.
 -    
 -    Therefore sum(multiples of 3 or multiples of 5) == 
 -       sum (muliples of 3) + sum (multiples of 5) - sum (multiples of 15)
 -}

import Control.Monad

sigma n = n * (n+1) `div` 2

sum_multiples_3_5 = do
	n <- readLn :: IO Int
	print $ 3*sigma((n-1) `div` 3) + 5*sigma((n-1) `div` 5) - 15*sigma((n-1) `div` 15)

main = do
	nCases <- readLn :: IO Int

	{- replicateM_ to discard [()...] as 'sum_multiples_3_5' returns "IO ()" -}
	replicateM_ nCases sum_multiples_3_5


{-
 - Test execution:
 -
 - [15:23:29 project_euler]$ echo -e "3\n10\n100\n1000" | runghc 1_multiples_of_3_5.hs 
 - 23
 - 2318
 - 233168
 -
 -}
