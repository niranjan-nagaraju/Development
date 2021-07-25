{-
 - Problem 14
 - (*) Duplicate the elements of a list.
 -
 - Example:
 -
 - * (dupli '(a b c c d))
 - (A A B B C C C C D D)
 - Example in Haskell:
 -
 - λ> dupli [1, 2, 3]
 - [1,1,2,2,3,3]
 -}


import Control.Exception( assert )

duplicate [] = []
duplicate (x:xs) = [x] ++ [x] ++ duplicate xs

-- alternate implementation using foldl
duplicate' :: [a] -> [a]
duplicate' = foldl (\acc x -> acc ++ [x,x]) []

-- alternate implementation using foldr
duplicate'' :: [a] -> [a]
duplicate'' = foldr (\x acc -> [x,x] ++ acc) []

main = do
	putStr $ assert ( (duplicate [1,2,3,4,5]) == [1,1,2,2,3,3,4,4,5,5] ) ""
	putStr $ assert ( (duplicate "abcde") == "aabbccddee" ) ""

	putStr $ assert ( (duplicate' [1,2,3,4,5]) == [1,1,2,2,3,3,4,4,5,5] ) ""
	putStr $ assert ( (duplicate' "abcde") == "aabbccddee" ) ""

	putStr $ assert ( (duplicate'' [1,2,3,4,5]) == [1,1,2,2,3,3,4,4,5,5] ) ""
	putStr $ assert ( (duplicate'' "abcde") == "aabbccddee" ) ""
