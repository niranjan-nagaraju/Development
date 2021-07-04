{- Eliminate consecutive duplicates of list elements. -}

import Control.Exception( assert )

compress [] = []
compress (x:xs) = 
	[x] ++ compress (dropWhile (==x) xs)

main = do
	putStr $ assert ( (compress ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]) == ["a", "b", "c", "a", "d", "e"] ) ""
