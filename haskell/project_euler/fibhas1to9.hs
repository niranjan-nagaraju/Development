import Data.Char
import Data.List

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

has1to9::Integer->Bool
has1to9 n = (first9 == "123456789") && (last9 == "123456789")
	where
		first9 = sort $ show (n `mod` 1000000000)
		last9 = sort $ take 9 $ show n

getFib = snd $ head $ dropWhile(\(x,y) -> not (has1to9 x)) (zip fibs [1..330000])

main = do
	-- print $ has1to9 1234567890123456789
	print $ getFib
