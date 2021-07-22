{-
 - (**) Decode a run-length encoded list.
 -
 - Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
 -
 - Example in Haskell:
 -
 - λ> decodeModified 
 -        [Multiple 4 'a',Single 'b',Multiple 2 'c',
 -         Multiple 2 'a',Single 'd',Multiple 4 'e']
 - "aaaabccaadeeee"
 -}

import Control.Exception( assert )

data EncodedData a = Multiple Int a | Single a
	deriving (Show)

enumerate (Single item) = [item]
enumerate (Multiple count item) = replicate count item

decodeModified = concatMap enumerate

main = do
	putStr $ assert (
		(decodeModified
			[Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
		) == "aaaabccaadeeee" ) ""

	putStr $ assert (
		(decodeModified
			[Multiple 4 1, Single 2, Single 3, Multiple 4 6]
		) == [1,1,1,1,2,3,6,6,6,6] ) ""
