import IO
import Ratio

triangle :: Double -> Int
triangle n = 
	floor ((n * (n+1)) / 2)

isWhole x = 
	numerator (frac) `mod` denominator (frac) == 0
	where
		frac = toRational x

isTriPentaHexa x = 
	isWhole(y) && isWhole(z)
	where
		c = x**2 + x
		y = (1 + sqrt (1+12*c)) / 6
		z = (1 + sqrt(1+4*c)) / 4

get_next_n_for_triangle = 
	head [x | x <- [286..], isTriPentaHexa(x)]

main = do
	let n = get_next_n_for_triangle
	print n
	print $ (triangle n)

	{- 55385.0
	 - 1533776805
	 -}
