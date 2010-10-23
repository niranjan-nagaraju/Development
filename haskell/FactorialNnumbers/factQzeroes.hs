import Control.Monad

fact :: Integer -> Integer
fact x = product [1..x]

xFactQZeroes:: Integer -> Integer
xFactQZeroes q = 
    head $ take 1 $ ([x| x<- [1..], ((fact x ) `mod` (10^q) == 0)])
	
main = do
	n <- readLn
	inputList <- forM [1..n] (\a -> readLn)
	
	let outputList = map xFactQZeroes inputList
	mapM_ print outputList
