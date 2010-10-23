import Control.Monad  
import Data.Bits

sumOfN :: Integer -> Integer
sumOfN x = shift (x * (x-1)) (-1);

main = do 	
	n <- readLn
	
	-- Read n numbers and store them in a list
	inputList <- forM [1..n] (\a -> readLn)
			
	let outputList = map sumOfN inputList
	mapM_ print outputList