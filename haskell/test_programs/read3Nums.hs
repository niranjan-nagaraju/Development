import Control.Monad

main = do
	[a,b,c] <- forM [1..3] (\x -> readLn::IO Integer)
	print a
	print b
	print c
