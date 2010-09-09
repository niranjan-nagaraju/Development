import IO

sum_of_divisors :: Int -> Int
sum_of_divisors n = 
	sum [x | x <- [1 .. (n `div` 2)], (n `mod` x == 0)]

abundant_numbers :: [Int]
abundant_numbers = 
	[x | x <- [1..28123], ((sum_of_divisors x) > x)]

numbers_sum_ab = 
	[(a+b) | a <- abundant_numbers, b<-abundant_numbers, (a+b <= 28123)] 

numbers_not_sum_ab = 
	[x | x <- [1..28123], (notElem x numbers_sum_ab)]

main = do
	print $ sum numbers_not_sum_ab

{- 4179871 -}
