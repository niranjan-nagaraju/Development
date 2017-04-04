{-
 - Return the sum of odd elements from the given list
 -
 - https://www.hackerrank.com/challenges/fp-sum-of-odd-elements
 -}

import Data.Bits

sum_odd_numbers arr = sum $ filter (\x -> (x .&. 1) == 1) arr

-- This part handles the Input/Output and can be used as it is. Do not change or modify it.
main = do
   inputdata <- getContents
   putStrLn $ show $ sum_odd_numbers $ map (read :: String -> Int) $ lines inputdata

{-
 - Sample run
 -
 - [15:59:06 functional_programming]$ printf "5\n4\n3\n2\n1\n0" | runghc sum_odd_numbers.hs 
 - 9
 -} 
