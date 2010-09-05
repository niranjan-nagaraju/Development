import Control.Monad

{- Solution to ax^2 + bx + c = 0 -}

quadraticRoots a b c =
	let det = sqrt (b*b - 4*a*c)
		-- two_a = 2 * a
	in	((-b + det) / (2*a),
		 (-b - det) / (2*a))

main = do
	a <- readLn
	b <- readLn
	c <- readLn
	print "Quadratic Roots: "
	print $ quadraticRoots a b c
