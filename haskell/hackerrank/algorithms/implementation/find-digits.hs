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

find_digits_dividing' :: Int -> Int -> Int -> Int
find_digits_dividing' _ 0 sum = sum
find_digits_dividing' num rest_of_num sum = 
	let digit = rest_of_num `mod` 10
	in
	if (digit == 0) || (num `mod` digit /= 0)
			then
				find_digits_dividing' num (rest_of_num `div` 10) sum
			else 
				find_digits_dividing' num (rest_of_num `div` 10) (sum + 1)

find_digits_dividing :: Int -> Int
find_digits_dividing num = 
	find_digits_dividing' num num 0



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

