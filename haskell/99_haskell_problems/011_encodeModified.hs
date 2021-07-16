{-
 - Problem 11:
 - (*) Modified run-length encoding.
 -
 - Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
 -
 - Example:
 -
 - * (encode-modified '(a a a a b c c a a d e e e e))
 - ((4 A) B (2 C) (2 A) D (4 E))
 - Example in Haskell:
 -
 - λ> encodeModified "aaaabccaadeeee"
 - [Multiple 4 'a',Single 'b',Multiple 2 'c',
 -  Multiple 2 'a',Single 'd',Multiple 4 'e']
 -}

import Control.Exception( assert )

data EncodedData a = Multiple Int a | Single a

-- custom show handler for EncodedData
showEncodedData :: (Show a) => (EncodedData a) -> String
showEncodedData (Single item) = show item
showEncodedData (Multiple count item) = "(" ++ (show count) ++ " " ++ (show item) ++ ")"

instance (Show a) => Show (EncodedData a) where 
	show = showEncodedData

-- encode from problem #10
encode :: [Char] -> [(Int,Char)]
encode [] = []
encode (x:xs) = (count, x) : encode (dropWhile (==x) xs)
	where
	count = length (takeWhile (==x) xs) + 1


-- Convert a single run-length encoded entry into `EncodedData` type
encodeModifiedItem :: (Int, a) -> (EncodedData a)
encodeModifiedItem (count, item)
	| count == 1 = (Single item)
	| otherwise = (Multiple count item)


-- Return a list of `EncodedData` with modified run-length encoding
encodeModified list = map encodeModifiedItem (encode list)

main = do
	putStr $ assert ( (show (Multiple 4 'C')) == "(4 'C')" ) ""
	putStr $ assert ( (show (Single "abc")) == "\"abc\"" ) ""
	putStr $ assert ( (show [(Multiple 10 'A'), (Single 'B'), (Multiple 4 'C')]) == "[(10 'A'),'B',(4 'C')]" ) ""

	putStr $ assert ( show (encodeModified "aaaabccaadeeee") == "[(4 'a'),'b',(2 'c'),(2 'a'),'d',(4 'e')]" ) ""
	putStr $ assert ( show (encodeModified "aaabbcdd") == "[(3 'a'),(2 'b'),'c',(2 'd')]" ) ""

