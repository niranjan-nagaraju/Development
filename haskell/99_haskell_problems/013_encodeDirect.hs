{-
 - Problem 13
 -
 - (**) Run-length encoding of a list (direct solution).
 -
 - Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
 -
 - Example:
 -
 - * (encode-direct '(a a a a b c c a a d e e e e))
 - ((4 A) B (2 C) (2 A) D (4 E))
 - Example in Haskell:
 -
 - λ> encodeDirect "aaaabccaadeeee"
 - [Multiple 4 'a',Single 'b',Multiple 2 'c',
 -  Multiple 2 'a',Single 'd',Multiple 4 'e']
 -}


import Control.Exception( assert )

data EncodedData a = Multiple Int a | Single a
	deriving (Show, Eq)

encodeCountWithLookahead :: (Eq a) => [a] -> [(Int, a)]
encodeCountWithLookahead = foldr lookAheadEncodeHelper []
	where
		lookAheadEncodeHelper x [] = [(1, x)] -- foldr goes R-L, so if acc is empty, we initialize it with (1,x)
		lookAheadEncodeHelper x (y:ys)
			| x == (snd y) = (1 + fst y, x) : ys -- lookahead 1 item, if ==, increase count in acc, and consume both items
			| otherwise = (1, x):y:ys -- x/=y => add (1,x) to accumulator, leave lookahead item, y, as-is

-- Convert a single run-length encoded entry into `EncodedData` type
encodeItem :: (Int, a) -> (EncodedData a)
encodeItem (1, item) = (Single item)
encodeItem (c, item) = (Multiple c item)

-- Use composiion to chain encodeItem and encodeCountWithLookAhead
-- So each (count, item) yielded is immediately converted to the
-- `EncodedData` type
encodeDirect :: (Eq a) => [a] -> [(EncodedData a)]
encodeDirect = map encodeItem . encodeCountWithLookahead 

main = do
	putStr $ assert ( (encodeCountWithLookahead "aaaabccaadeeee") == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')] ) ""
	putStr $ assert ( (encodeCountWithLookahead "aaabbcdd") == [(3,'a'),(2,'b'),(1,'c'),(2,'d')] ) ""

	putStr $ assert ( (encodeDirect "aaaabccaadeeee") ==
		[Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e'] ) ""
	putStr $ assert ( (encodeDirect "aaabbcdd") ==  [Multiple 3 'a',Multiple 2 'b',Single 'c',Multiple 2 'd'] ) ""
