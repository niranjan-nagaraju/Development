import Control.Exception (assert)


gcd' m 0 = m
gcd' m n = gcd' n (m `mod` n)

-- Testcases                                                                                                                                                 
test_gcd :: [Char]
test_gcd =
	assert (gcd' 36 24 == 12) "" ++
	assert (gcd' 36 0 == 36) ""  ++
	assert (gcd' 2 3 == 1) "" 
	

main = do
	putStrLn  $ test_gcd ++ "Testcases passed!"
