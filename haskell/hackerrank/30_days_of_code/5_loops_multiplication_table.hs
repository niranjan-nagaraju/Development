{-
 - https://www.hackerrank.com/challenges/30-loops
 -} 

print_table :: Int -> Int -> IO()
print_table n i =
	putStrLn (show(n) ++ " x " ++ show(i) ++ " = " ++ show(n*i))

main = do
	n <- readLn :: IO Int
	mapM_ (print_table n) [1..10]

