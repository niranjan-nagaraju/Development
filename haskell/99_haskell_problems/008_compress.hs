{- Eliminate consecutive duplicates of list elements. -}

import Control.Exception( assert )

compress [] = []
compress (x:xs) = 
	[x] ++ compress (dropWhile (==x) xs)


-- alternate implementation using fold
-- add current item to accumulator only if it doesn't match the accumulator's last item
compress' list = foldl (\acc x -> if acc == [] || x /= (last acc) then acc++[x] else acc ) [] list


main = do
	putStr $ assert ( (compress "") == "" ) ""
	putStr $ assert ( (compress "aaaabccaadeeee") == "abcade" ) ""
	putStr $ assert ( (compress [1,1,1,2,2,3,3,3,3,4,3,4]) == [1,2,3,4,3,4] ) ""

	putStr $ assert ( (compress' "") == "" ) ""
	putStr $ assert ( (compress' "aaaabccaadeeee") == "abcade" ) ""
	putStr $ assert ( (compress' [1,1,1,2,2,3,3,3,3,4,3,4]) == [1,2,3,4,3,4] ) ""
