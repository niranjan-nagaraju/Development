{- Withdrawals should be multiple of 5 and no -ve balances -}

import IO

-- withdraw :: (Fractional t) -> (Fractional t) -> (Fractional t)
withdraw amt balance = 
	if (amt > balance) || ((truncate amt) `mod` 5 /= 0) || (amt+0.50 > balance)
		then
			return balance
		else 
			return (balance - amt - 0.50)

parseInput :: String -> [Double]
parseInput str = map read.words $ str

main = do
	str <- (getLine::IO String)
	let [amt, balance] = parseInput str
	-- amt <- readLn
	-- balance <- readLn

	nBalance <- withdraw amt balance
	putStrLn $ (show nBalance ++ "0")
