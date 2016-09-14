{-
 - https://www.hackerrank.com/challenges/30-operators
 -}

main = do
	mealCost <- readLn :: IO Float
	tipPercent <- readLn :: IO Float
	taxPercent <- readLn :: IO Float

	let totalCost = round (mealCost + (mealCost * (tipPercent+taxPercent) / 100.0))
	putStrLn ("The total meal cost is " ++ show(totalCost) ++ " dollars.")
	
