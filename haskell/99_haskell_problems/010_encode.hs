{-
 - Run-length encoding of a list. 
 - Use the result of problem P09 to implement the so-called run-length encoding
 - data compression method. 
 - Consecutive duplicates of elements are encoded as lists (N E) 
 - where N is the number of duplicates of the element E
 -
 - encode "aaaabccaadeeee" => [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
 -} 

import Control.Exception( assert )

-- Pack function from P09
pack [] = []
pack (x:xs) = 
	[[x] ++ (takeWhile (==x) xs)]  ++ pack (dropWhile (==x) xs)

-- Compress function from P08
compress [] = []
compress (x:xs) = 
	[x] ++ compress (dropWhile (==x) xs)

encode list = 
	zip (map length packedList) compressedList
	where
	packedList = (pack list)
	compressedList = (compress list)


-- alternate implementation
encode' :: [Char] -> [(Int,Char)]
encode' [] = []
encode' (x:xs) = (count, x) : encode' (dropWhile (==x) xs)
	where
	count = length (takeWhile (==x) xs) + 1

main = do
	putStr $ assert ( (encode "aaaabccaadeeee") == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')] ) ""
	putStr $ assert ( (encode' "aaaabccaadeeee") == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')] ) ""

