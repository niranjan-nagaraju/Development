main = do x <- aaa
	  print x
	  
aaa :: IO (Int, Int, [Int])	  
aaa = do x <- readLn
	 return (sum x, product x, x)
