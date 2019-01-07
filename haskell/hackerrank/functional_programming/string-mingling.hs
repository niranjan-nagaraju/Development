{-
 - https://www.hackerrank.com/challenges/string-mingling/problem
 -
 - Sample Input #00
 - abcde
 - pqrst
 -
 - Sample Output #00
 - apbqcrdset
 -
 -}


import Control.Exception (assert)

{- times out, doesn't yield well to lazy evaluation -}
mingle'' p q = foldr (\x acc -> (p !! x):(q !! x):acc) "" [0..(length p - 1)]

{- Use zip instead of list indexing to create pairs of characters, and then concatenate them into one string -} 
mingle p q = foldr (\x acc -> fst x: snd x: acc) ""  [(x, y) | (x, y) <- zip p q]


{- basic testcases -}
test_mingle =
	let p = "abcde"; q = "pqrst"
	in assert ((mingle p q) == "apbqcrdset") "" ++
	let p = "hacker"; q = "ranker"
	in assert ((mingle p q) == "hraacnkkeerr") "" 


main = do
	{- run basic testcases -}
	putStr test_mingle

	{- mingle two input strings -}
	s1 <- getLine
	s2 <- getLine
	putStrLn $ mingle s1 s2 

