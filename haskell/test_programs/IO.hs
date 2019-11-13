import Control.Monad

main = do 	
	n <- readLn
	
	-- Read n numbers and store them in a list
	inputList <- forM [1..n] (\a -> readLn)
			
	let outputList = map (countZeroes.show.factorial) inputList
	mapM_ print outputList