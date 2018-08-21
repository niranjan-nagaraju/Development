import IO

isBinPalindrome :: Int -> Bool
isBinPalindrome x = 
	bin == (reverse bin)
    where
	 	decToBin' 0 = []
		decToBin' y = 
			let (a,b) = quotRem y 2 
			in 
			[b] ++ decToBin' a

		bin = (decToBin' x)

isDecPalindrome :: Int -> Bool
isDecPalindrome x = 
	xStr == (reverse xStr)
	where
		xStr = show x

get_DecBinPalindromes =
	filter (\x -> (isDecPalindrome x && isBinPalindrome x)) [1 .. 1000000]

main = do
	let decbinpals = get_DecBinPalindromes
	print $ decbinpals
	print $ sum decbinpals

{-
 - [1,3,5,7,9,33,99,313,585,717,7447,9009,15351,32223,39993,53235,53835,73737,585585]
 - 872187
 -}
