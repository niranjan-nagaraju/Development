import Control.Monad

countZeroes :: [Char] -> Int
countZeroes mylist = 
	if (last mylist) /= '0'
		then 0
		else 1 + (countZeroes $ init mylist)
		

factorial x = product [1..x]

main = do 	
	n <- readLn
	
	-- Read n numbers and store them in a list
	inputList <- forM [1..n] (\a -> readLn)
			
	let outputList = map (countZeroes.show.factorial) inputList
	mapM_ print outputList
