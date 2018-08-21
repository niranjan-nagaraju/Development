import IO
import TriangleWords_42
import Char

{-
14 is the max characters
so max triangle word can be 14 * 26 = 364 < sigma(26)
-}

triangleNumbers :: [Int]
triangleNumbers = map (\x -> (x*(x+1)) `div` 2) [1..26]

sum_of_chars :: String -> Int
sum_of_chars inword = foldl (\x y -> x + (ord(y) - ord('A')) + 1) 0 inword

num_triangle_words = 
	length (filter (\x -> (elem (sum_of_chars x) triangleNumbers)) inWords)

main = do
	print num_triangle_words 
	{- 162 -}
