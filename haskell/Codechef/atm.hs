{- 
 - Withdrawals should be multiple of 5 and no -ve balances 
 - https://www.codechef.com/problems/HS08TEST
 -}

import System.IO


withdraw amt balance 
	| (amt < 0) || (balance < 0) = return balance
	| (amt > balance) || ((truncate amt) `mod` 5 /= 0) || (amt+0.50 > balance) = return balance
	| otherwise = return (balance - amt - 0.50)
		
			

parseInput :: String -> [Double]
parseInput str = map read.words $ str

main = do
	str <- (getLine::IO String)
	let [amt, balance] = parseInput str
	-- amt <- readLn
	-- balance <- readLn

	nBalance <- withdraw amt balance
	putStrLn $ (show nBalance ++ "0")
