import Data.List
import Data.Char

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

isFibPan n =
	let 
		a = n `mod` 1000000000
		b = sort $ show a
		c = sort $ take 9 $ show n
	in  b == "123456789" && c == "123456789"
				  
ex_104 = snd $ head $ dropWhile (\(x,y) -> (not . isFibPan) x) (zip fibs [1..])

main = do
	print $ ex_104
