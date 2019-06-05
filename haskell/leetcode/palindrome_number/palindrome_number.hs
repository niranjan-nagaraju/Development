{-
https://leetcode.com/problems/palindrome-number/

Sample Input
3
12
121
-121

Sample Output
False
True
False
-}

is_palindrome :: Int -> Bool
is_palindrome n
	| (n < 0) = False
	| (n < 10) = True
	| otherwise  = (nStr == reverse nStr)
	where nStr = show n
	

-- Read n lines and process separately
getLinesAndProcess :: Int -> IO ()
getLinesAndProcess 0 = return ()
getLinesAndProcess n = do
	num <- readLn :: IO Int
	putStrLn $ show (is_palindrome num)
	getLinesAndProcess (n-1)

main = do
	n <- readLn :: IO Int
	getLinesAndProcess n


{-
 - sample run:
 - 
 - $ echo -e "3\n1\n-11\n121" | runghc leetcode/palindrome_number/palindrome_number.hs
 - True
 - False
 - True
-}
