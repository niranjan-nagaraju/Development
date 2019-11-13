import Control.Monad


main = do
	n <- getLine
	let number = (read n)::Int
	
	inputList <- mapM  (\a -> do
		nStr <- getLine
		let n = (read nStr)::Int
		return n) [1..number]
	
	--let inputList = getNintegers number
	print inputList 
	

--getNintegers :: Int -> [String]
--getNintegers 0 = []
--getNintegers x = do
--	nStr <- getLine
--	nStr : getNintegers (x-1)
	