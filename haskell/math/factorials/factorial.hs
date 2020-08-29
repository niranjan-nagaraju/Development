import Control.Monad  

factorial :: Integer -> Integer
factorial x = product [1..x]

main = do 	
	n <- readLn
	
	-- Read n numbers and store them in a list
	inputList <- forM [1..n] (\a -> readLn)
			
	let outputList = map factorial inputList
	mapM_ print outputList

