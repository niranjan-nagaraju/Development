{-
 - Problem 16
 -
 - (**) Drop every N'th element from a list.
 -
 - Example:
 -
 - * (drop '(a b c d e f g h i k) 3)
 - (A B D E G H K)
 - Example in Haskell:
 -
 - λ> dropEvery "abcdefghik" 3
 - "abdeghk"
 -}

import Control.Exception( assert )

dropEvery list 0 = list
dropEvery list n = 
	dropEvery' list n 1
	where
	dropEvery' [] _ _ = []
	dropEvery' (x:xs) n index = 
		if index == n
			then dropEvery' xs n 1 
			else x : dropEvery' xs n (index+1)


-- using zip to enumerate, and list comprehensions.
dropEvery' list 0 = list
dropEvery' list n = [ x | (idx,x) <- zip [1..] list, idx `mod` n /= 0 ]

-- using `take` and `drop`
dropEvery'' :: [a] -> Int -> [a]
dropEvery'' [] _ = []
dropEvery'' list 0 = list
dropEvery'' list n = (take (n-1) list) ++ dropEvery'' (drop n list)  n

main = do
	putStr $ assert ( (dropEvery "abcdefghijk" 0) == ['a'..'k'] ) ""
	putStr $ assert ( (dropEvery' "abcdefghijk" 0) == ['a'..'k'] ) ""
	putStr $ assert ( (dropEvery'' "abcdefghijk" 0) == ['a'..'k'] ) ""

	putStr $ assert ( (dropEvery "abcdefghijk" 3) == "abdeghjk" ) ""
	putStr $ assert ( (dropEvery' "abcdefghijk" 3) == "abdeghjk" ) ""
	putStr $ assert ( (dropEvery'' "abcdefghijk" 3) == "abdeghjk" ) ""

	putStr $ assert ( (dropEvery "abcdefghijk" 1) == "" ) ""
	putStr $ assert ( (dropEvery' "abcdefghijk" 1) == "" ) ""
	putStr $ assert ( (dropEvery'' "abcdefghijk" 1) == "" ) ""

	putStr $ assert ( (dropEvery "abcdefghik" 3) == "abdeghk" ) ""
	putStr $ assert ( (dropEvery' "abcdefghik" 3) == "abdeghk" ) ""
	putStr $ assert ( (dropEvery'' "abcdefghik" 3) == "abdeghk" ) ""

	putStr $ assert ( (dropEvery "abcdefghik" 2) == "acegi" ) ""
	putStr $ assert ( (dropEvery' "abcdefghik" 2) == "acegi" ) ""
	putStr $ assert ( (dropEvery'' "abcdefghik" 2) == "acegi" ) ""

