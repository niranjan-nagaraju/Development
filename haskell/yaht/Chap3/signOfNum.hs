import Control.Monad

signOfNum x =
	if x < 0
		then -1
		else if x > 0
			then 1
			else 0

main = do
	print $ signOfNum (5) 
	print $ signOfNum (-6) 
	print $ signOfNum (0) 
