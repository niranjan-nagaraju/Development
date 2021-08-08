{- Problem 19
 -
 - Rotate a list N places to the left.
 - On -ve N, rotate (len-N) places to the left
 -
 - Trial Run:
 -	 *Main> rotate ['a','b','c','d','e','f','g','h'] 3
 -   "defghabc"
 -  
 -   *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
 -   "ghabcdef"
 -}

import Control.Exception( assert )

rotate list n 
	| (n == 0) = list
	| (n > 0) = rotate' list n
	| otherwise = rotate' list (length list + n)	-- n < 0
	where
	rotate' list n = 
		(drop n list) ++ (take n list)


rotate' list n
	| (n > 0) = (drop n list) ++ (take n list) -- rotate left on +ve shift
	| (n < 0) = (drop (len+n) list) ++ (take (len+n) list) -- rotate right on -ve shift
	| otherwise = list -- n == 0
	where len = length list


{- using mod -}
rotate'' list n = (drop len list) ++ (take len list)
	where len = mod n (length list)


{-
 - Replicate list thrice
 - "abcdefgh" -> "abcdefghabcdefghabcdefgh"
 - rotate 3 -> drop (len + 3), -> "defghabcdefghabcdefgh"
 -          -> take (len)  -> "defghabc"
 -
 -  rotate (-2) -> drop (len - 2) -> "ghabcdefghabcdefgh"
 -				-> take (len) -> "ghabcdef"
 -}
rotate''' list n = take len $ drop (len+n) (list++list++list)
	where len = length list


-- alternately, use `cycle` which endlessly concatenates itself to `replicate`
rotate'''' list n = take len $ drop (len+n) $ cycle list
	where len = length list



{- using rotateLeft or +ve shift, and rotateRight for -ve shifts.
 -  rotateLeft: starts with (left, right) == (list, [])
 -    Transfer head of left to right's tail until right-list has `n` items
 -  "abcdefgh" -> rotateLeft 3 == ("defgh" ++ "abc")
 -
 - rotateRight is a mirror image of rotateLeft.
 -  rotateRight: starts with (left, right) == ([], list)
 -    Transfer end of right to left's head until left-list has `n` items
 -  "abcdefgh" -> rotateRight 3 == ("fgh" ++ "abcde")
 -  == rotateLeft 5
 -}
rotate5 [] n = []
rotate5 list n
	| n > 0 = rotateLeft (list, []) n
	| n < 0 = rotateRight ([], list) (-n)
	| otherwise = list -- n==0
	where
	rotateLeft (left, right) n
		| length right == n = (left ++ right)
		| otherwise = rotateLeft (tail left, right ++ [head left]) n
	rotateRight (left, right) n
		| length left == n = (left ++ right)
		| otherwise = rotateRight ([last right] ++ left, init right) n


{- 
 - rotate6 is the same as rotate5 but avoids using `length`
 - by counting down `n` each time we move an item from
 - left to right or vice-versa
 -}
rotate6 [] n = []
rotate6 list n
	| n > 0 = rotateLeft (list, []) n
	| n < 0 = rotateRight ([], list) (-n)
	| otherwise = list -- n==0
	where
	rotateLeft (left, right) n
		| n == 0 = (left ++ right)
		| otherwise = rotateLeft (tail left, right ++ [head left]) (n-1)
	rotateRight (left, right) n
		| n == 0 = (left ++ right)
		| otherwise = rotateRight ([last right] ++ left, init right) (n-1)

{- 
 - rotate7 simplifies rotate6
 - by keeping a single list, instead of separating them into (left, right)
 - rotateLeft/right by shuffling first/last element to end/front one item at a time
 -}
rotate7 [] _ = []
rotate7 list n
	| n > 0 = rotate7 ( (tail list) ++ [head list] ) (n-1) -- rotate left
	| n < 0 = rotate7 ( [last list] ++ (init list) )  (n+1) -- rotate right
	| otherwise = list -- n==0

main = do
	putStr $ assert ( (rotate "abcdefgh" 3) == "defghabc" ) "" 
	putStr $ assert ( (rotate "abcdefgh" (-2)) == "ghabcdef" ) ""

	putStr $ assert ( (rotate' "abcdefgh" 3) == "defghabc" ) "" 
	putStr $ assert ( (rotate' "abcdefgh" (-2)) == "ghabcdef" ) ""

	putStr $ assert ( (rotate'' "abcdefgh" 3) == "defghabc" ) "" 
	putStr $ assert ( (rotate'' "abcdefgh" (-2)) == "ghabcdef" ) ""

	putStr $ assert ( (rotate''' "abcdefgh" 3) == "defghabc" ) "" 
	putStr $ assert ( (rotate''' "abcdefgh" (-2)) == "ghabcdef" ) ""

	putStr $ assert ( (rotate'''' "abcdefgh" 3) == "defghabc" ) "" 
	putStr $ assert ( (rotate'''' "abcdefgh" (-2)) == "ghabcdef" ) ""

	putStr $ assert ( (rotate5 "abcdefgh" 3) == "defghabc" ) "" 
	putStr $ assert ( (rotate5 "abcdefgh" (-2)) == "ghabcdef" ) ""

	putStr $ assert ( (rotate6 "abcdefgh" 3) == "defghabc" ) "" 
	putStr $ assert ( (rotate6 "abcdefgh" (-2)) == "ghabcdef" ) ""

	putStr $ assert ( (rotate7 "abcdefgh" 3) == "defghabc" ) "" 
	putStr $ assert ( (rotate7 "abcdefgh" (-2)) == "ghabcdef" ) ""

