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

