{-
 - Problem 15
 - (**) Replicate the elements of a list a given number of times.
 -
 - Example:
 -
 - * (repli '(a b c) 3)
 - (A A A B B B C C C)
 - Example in Haskell:
 -
 - λ> repli "abc" 3
 - "aaabbbccc"
 -}

import Control.Exception( assert )

-- replicate a single item by the specified times.
-- repliItem 1 3 -> [1,1,1]
repliItem x times = map (\_ -> x) [1..times]


repli :: (Show a) => [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) times = (repliItem x times) ++ (repli xs times)

-- alternate implementation using foldl
repli' :: (Show a) => [a] -> Int -> [a]
repli' list times = foldl (\acc x -> acc ++ repliItem x times) [] list

-- alternate implementation using foldr
repli'' :: (Show a) => [a] -> Int -> [a]
repli'' list times = foldr (\x acc -> repliItem x times ++ acc) [] list



main = do
	putStr $ assert ( (repli "abc" 3) == "aaabbbccc" ) ""
	putStr $ assert ( (repli [1,2,3,4] 4) == [1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4] ) ""
	putStr $ assert ( (repli "abcd" 1) == "abcd" ) ""

	putStr $ assert ( (repli' "abc" 3) == "aaabbbccc" ) ""
	putStr $ assert ( (repli' [1,2,3,4] 4) == [1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4] ) ""
	putStr $ assert ( (repli' "abcd" 1) == "abcd" ) ""

	putStr $ assert ( (repli'' "abc" 3) == "aaabbbccc" ) ""
	putStr $ assert ( (repli'' [1,2,3,4] 4) == [1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4] ) ""
	putStr $ assert ( (repli'' "abcd" 1) == "abcd" ) ""
