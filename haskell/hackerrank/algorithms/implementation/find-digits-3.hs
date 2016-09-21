{-
https://www.hackerrank.com/challenges/find-digits

Number of digit of N that evenly divide N

Sample Input
2
12
1012

Sample Output
2
3
-}

-- Return an array of digits for a given number, 1234 -> [4,3,2,1]
get_digits :: Int -> [Int]
get_digits num = 
	let decreasing_digits = takeWhile (\x -> x /= 0) [(num `div` (10 ^ x)) | x <- [0..]] -- 1234 -> [1234, 123, 12, 1]
	in map (\x -> x `mod` 10) decreasing_digits -- [4, 3, 2, 1]


find_digits_dividing :: Int -> Int
find_digits_dividing num = 
	sum [1 | x <- digits,  x /= 0, (num `mod` x == 0)]  -- List of 1s counting every digit evenly dividing 'num'
	where
		digits = get_digits num
	


-- Read n lines and process separately
getLinesAndProcess :: Int -> IO ()
getLinesAndProcess 0 = return ()
getLinesAndProcess n = do
	num <- readLn :: IO Int
	putStrLn ( show (find_digits_dividing num) )
	getLinesAndProcess (n-1)

main = do
	n <- readLn :: IO Int
	getLinesAndProcess n

